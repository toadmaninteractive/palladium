-module(web_analytics).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("clickhouse/include/clickhouse_protocol.hrl").
-include("session.hrl").
-include("card_protocol.hrl").
-include("common_protocol.hrl").
-include("data_protocol.hrl").
-include("visualization_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    is_project_database/2,
    project_data/1,
    project_metrics/1,
    project_maps/1,
    project_queries/1,
    project_query/2,
    project_query_params/2,
    project_query_params/3,
    project_query_param/3,
    project_clickhouse_server/1,
    send_query/8,
    dynamic_values/5,
    project_db/1,
    to_dict/3
]).

-define(config_card, config).

%% API

-spec is_project_database(ProjectId :: binary(), Database :: binary()) ->
    boolean().

is_project_database(ProjectId, Database) ->
    Db = project_db(ProjectId),
    #card_config{databases = DBs} = cards:card_config(Db, ?config_card),
    lists:member(Database, DBs).

-spec project_data(ProjectId :: binary()) ->
    web_protocol:analytics_data().

project_data(ProjectId) ->
    Db = project_db(ProjectId),
    #card_config{
        metrics = MetricCardKeys,
        maps = MapCardKeys,
        parameters = ParamCardKeys,
        queries = QueryCardKeys
    } = cards:card_config(Db, ?config_card),
    Metrics = [cards:card_metric(Db, CardKey) || CardKey <- MetricCardKeys],
    Maps = [cards:card_map(Db, CardKey) || CardKey <- MapCardKeys],
    Queries = [cards:card_query(Db, CardKey) || CardKey <- QueryCardKeys],
    Params = [cards:card_query_param(Db, CardKey) || CardKey <- lists:usort(ParamCardKeys)],
    #analytics_data{metrics = Metrics, maps = Maps, params = Params, queries = Queries}.

-spec project_metrics(ProjectId :: binary()) ->
    [card_protocol:card_metric()].

project_metrics(ProjectId) ->
    Db = project_db(ProjectId),
    #card_config{metrics = MetricCardKeys} = cards:card_config(Db, ?config_card),
    [cards:card_metric(Db, MCK) || MCK <- MetricCardKeys].

-spec project_maps(ProjectId :: binary()) ->
    [card_protocol:card_map()].

project_maps(ProjectId) ->
    Db = project_db(ProjectId),
    #card_config{maps = MapCardKeys} = cards:card_config(Db, ?config_card),
    [cards:card_map(Db, MCK) || MCK <- MapCardKeys].

-spec project_queries(ProjectId :: binary()) ->
    [card_protocol:card_query()].

project_queries(ProjectId) ->
    Db = project_db(ProjectId),
    #card_config{queries = QueryCardKeys} = cards:card_config(Db, ?config_card),
    [cards:card_query(Db, QCK) || QCK <- QueryCardKeys].

-spec project_query(ProjectId :: binary(), QueryKey :: binary()) ->
    card_protocol:card_query() | 'undefined'.

project_query(ProjectId, QueryKey) ->
    Queries = project_queries(ProjectId),
    case lists:keyfind(QueryKey, #card_query.key, Queries) of
        false -> undefined;
        Query -> Query
    end.

-spec project_query_params(ProjectId :: binary(), QueryKey :: binary()) ->
    [card_protocol:card_query_param()].

project_query_params(ProjectId, QueryKey) ->
    Db = project_db(ProjectId),
    #card_query{params = QueryParamKeys} = project_query(ProjectId, QueryKey),
    [cards:card_query_param(Db, QPK) || QPK <- QueryParamKeys].

-spec project_query_params(ProjectId :: binary(), QueryKey :: binary(), ParamKeys :: [binary()]) ->
    [card_protocol:card_query_param()].

project_query_params(ProjectId, QueryKey, ParamKeys) ->
    QueryParams = project_query_params(ProjectId, QueryKey),
    [QP || #card_query_param{key = QPK} = QP <- QueryParams, lists:member(QPK, ParamKeys)].

-spec project_query_param(ProjectId :: binary(), QueryKey :: binary(), ParamKey :: binary()) ->
    card_protocol:card_query_param() | 'undefined'.

project_query_param(ProjectId, QueryKey, ParamKey) ->
    QueryParams = project_query_params(ProjectId, QueryKey),
    case lists:keyfind(ParamKey, #card_query_param.key, QueryParams) of
        false -> undefined;
        Param -> Param
    end.

-spec project_clickhouse_server(ProjectId :: binary()) ->
    card_protocol:clickhouse_server().

project_clickhouse_server(ProjectId) ->
    Db = web_analytics:project_db(ProjectId),
    card_config:clickhouse_server(Db).

-spec send_query(ClickhouseServer, Database, RequestJson, Filters, Params, RequiredParamCardKeys, Sql, Output) -> Result when
    ClickhouseServer :: atom(),
    Database :: binary(),
    RequestJson :: jsx:json_term(),
    Filters :: [card_protocol:card_query_param()],
    Params :: [card_protocol:card_query_param()],
    RequiredParamCardKeys :: [card_protocol:query_param_key()],
    Sql :: binary(),
    Output :: visualization_protocol:visualization(),
    Result :: [data_protocol:numeric_series() | data_protocol:heatmap_series() | data_protocol:killmap_series()] | 'undefined'.

send_query(ClickhouseServer, Database, RequestJson, Filters, Params, RequiredParamCardKeys, Sql, Output) ->
    try
        do_send_query(ClickhouseServer, Database, RequestJson, Filters, Params, RequiredParamCardKeys, Sql, Output)
    catch Type:What:StackTrace ->
        logger:error("Analytics query failed (reason: ~p:~p)", [Type, What], #{caption => ?MODULE, stacktrace => StackTrace}),
        undefined
    end.

-spec dynamic_values(ClickhouseServer, Database, Sql, Params, RequestJson) -> Result when
    ClickhouseServer :: atom(),
    Database :: binary(),
    Sql :: binary(),
    Params :: [card_protocol:card_query_param()],
    RequestJson :: jsx:json_term(),
    Result :: [data_protocol:json_point()].

dynamic_values(ClickhouseServer, Database, Sql, Params, RequestJson) ->
    Sql1 = interpolate_sql(Params, [], RequestJson, Sql),
    Sql2 = strip_optional_blocks(Sql1),
    Sql3 = binary:replace(Sql2, [<<"\r">>, <<"\n">>], <<" ">>, [global]),
    nomatch = sanitize(Sql3),
    BaseUrl = clickhouse_config:url(ClickhouseServer),
    Username = clickhouse_config:username(ClickhouseServer),
    Password = clickhouse_config:password(ClickhouseServer),
    BasicAuth = clickhouse_auth:basic(Username, Password),
    #query_result{data = Data} = clickhouse_api:send_query(BaseUrl, Database, Sql3, BasicAuth),
    lists:map(fun data_protocol:json_point_from_json/1, Data).

-spec project_db(ProjectId :: binary()) -> atom().

project_db(ProjectId) ->
    binary_to_existing_atom(ProjectId, utf8).

-spec to_dict(Db, CardKeys, ParseFun) -> Result when
    Db :: atom(),
    CardKeys :: [atom()],
    ParseFun :: fun(),
    Result :: dict:dict(atom(), tuple()).

to_dict(Db, CardKeys, ParseFun) ->
    lists:foldl(fun(CardKey, AccDict) ->
        dict:store(CardKey, ParseFun(Db, CardKey), AccDict)
    end, dict:new(), CardKeys).

%% Local functions

do_send_query(ClickhouseServer, Database, RequestJson, Filters, Params, RequiredParamCardKeys, Sql, Output) ->
    Sql1 = interpolate_sql(Filters ++ Params, RequiredParamCardKeys, RequestJson, Sql),
    Sql2 = strip_optional_blocks(Sql1),
    Sql3 = clickhouse_macro:interpolate(Sql2),
    Sql4 = binary:replace(Sql3, [<<"\r">>, <<"\n">>], <<" ">>, [global]),
    nomatch = sanitize(Sql4),
    BaseUrl = clickhouse_config:url(ClickhouseServer),
    Username = clickhouse_config:username(ClickhouseServer),
    Password = clickhouse_config:password(ClickhouseServer),
    BasicAuth = clickhouse_auth:basic(Username, Password),
    #query_result{data = Data} = clickhouse_api:send_query(BaseUrl, Database, Sql4, BasicAuth),
    case Output of
        % Indicator
        #visualization_indicator{} ->
            Points = [#numeric_point{label = L, value = 1.0 * ?yesno(is_number(V), V, 0)} || #{<<"label">> := L, <<"value">> := V} <- Data],
            [#numeric_series{data = Points}];

        % Common plot
        #visualization_common_plot{series = Series} ->
            [begin
                Key1 = ?yesno(is_binary(Key), Key, <<"value">>),
                #numeric_series{
                    key = Key,
                    data = [begin
                        Value = maps:get(Key1, Item, 0),
                        #numeric_point{label = L, value = 1.0 * ?yesno(is_number(Value), Value, 0)}
                    end || #{<<"label">> := L} = Item <- Data]
                }
            end || #chart_serie{key = Key} <- Series];

        % Synchronized plot
        #visualization_synchronized_plot{series = Series} ->
            [begin
                Key1 = ?yesno(is_binary(Key), Key, <<"value">>),
                #numeric_series{
                    key = Key,
                    data = [begin
                        Value = maps:get(Key1, Item, 0),
                        #numeric_point{label = L, value = 1.0 * ?yesno(is_number(Value), Value, 0)}
                    end || #{<<"label">> := L} = Item <- Data]
                }
            end || #chart_serie{key = Key} <- Series];

        % Horizontal bars
        #visualization_horizontal_bars{series = Series} ->
            [begin
                Key1 = ?yesno(is_binary(Key), Key, <<"value">>),
                #numeric_series{
                    key = Key,
                    data = [begin
                        Value = maps:get(Key1, Item, 0),
                        #numeric_point{label = L, value = 1.0 * ?yesno(is_number(Value), Value, 0)}
                    end || #{<<"label">> := L} = Item <- Data]
                }
            end || #simple_serie{key = Key} <- Series];

        % Funnel
        #visualization_funnel{series = Series} ->
            [begin
                Key1 = ?yesno(is_binary(Key), Key, <<"value">>),
                #numeric_series{
                    key = Key,
                    data = [begin
                        Value = maps:get(Key1, Item, 0),
                        #numeric_point{label = L, value = 1.0 * ?yesno(is_number(Value), Value, 0)}
                    end || #{<<"label">> := L} = Item <- Data]
                }
            end || #simple_serie{key = Key} <- Series];

        % Heatmap
        #visualization_heatmap{} ->
            Maps = lists:usort([MapId || #{<<"map">> := MapId} <- Data]),
            Points = lists:map(fun data_protocol:point2_d_from_json/1, Data),
            [#heatmap_series{key = <<"heatmap">>, maps = Maps, data = Points}];

        % Killmap
        #visualization_killmap{} ->
            Map = case Data of
                [#{<<"map">> := M} | _] -> M;
                _ -> <<>>
            end,
            Points = lists:map(fun data_protocol:kill_point_from_json/1, Data),
            [#killmap_series{key = <<"killmap">>, map = Map, data = Points}];

        % Scatter plot
        #visualization_scatter{series = Series} ->
            [begin
                ToScatterPoint = fun(#{<<"x">> := X, <<"y">> := Y, <<"value">> := Value}) ->
                    #scatter_point{x = X, y = Y, value = 1.0 * ?yesno(is_number(Value), Value, 0)}
                end,
                ScatterPoints = case Key of
                    K when is_binary(K) -> [ToScatterPoint(Item) || #{<<"type">> := Type} = Item <- Data, Type =:= Key];
                    _ -> lists:map(ToScatterPoint, Data)
                end,
                #scatter_series{key = Key, data = ScatterPoints}
            end || #scatter_serie{key = Key} <- Series];

        % Box plot
        #visualization_box_plot{} ->
            ToBoxPlotPoint = fun(#{<<"label">> := Label, <<"value">> := Value}) ->
                #box_plot_point{label = Label, value = [1.0 * ?yesno(is_number(V), V, 0) || V <- Value]}
            end,
            BoxPlotPoints = [ToBoxPlotPoint(P) || P <- Data],
            [#box_plot_series{key = <<"box_plot">>, data = BoxPlotPoints}];

        % Raw data
        #visualization_raw{} ->
            [#raw_series{data = Data}]
    end.

%% Already interpolated

interpolate_sql([], _RequiredParamCardKeys, _Json, Sql) ->
    Sql;

%% Boolean

interpolate_sql([#card_query_param{id = CardKey, type = boolean = Type, key = ParamKey}|Rest], RequiredParamCardKeys, Json, Sql) ->
    IsRequired = lists:member(CardKey, RequiredParamCardKeys),
    case maps:get(ParamKey, Json, undefined) of
        undefined when IsRequired ->
            erlang:throw({error, {required_param_missing, ParamKey}});
        undefined ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql);
        B when is_boolean(B) ->
            BoolStr = ?yesno(B, <<"TRUE">>, <<"FALSE">>),
            Sql1 = binary:replace(Sql, <<"{", ParamKey/binary, "}">>, BoolStr, [global]),
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql1);
        _ -> erlang:throw({error, {param_type_mismatch, Type}})
    end;

%% Numeric

interpolate_sql([#card_query_param{id = CardKey, type = numeric = Type, key = ParamKey}|Rest], RequiredParamCardKeys, Json, Sql) ->
    IsRequired = lists:member(CardKey, RequiredParamCardKeys),
    case maps:get(ParamKey, Json, undefined) of
        undefined when IsRequired ->
            erlang:throw({error, {required_param_missing, ParamKey}});
        undefined ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql);
        N when is_number(N) ->
            NumStr = ?yesno(is_float(N), float_to_binary(N), integer_to_binary(N)),
            Sql1 = binary:replace(Sql, <<"{", ParamKey/binary, "}">>, NumStr, [global]),
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql1);
        _ ->
            erlang:throw({error, {param_type_mismatch, Type}})
    end;

%% Month

interpolate_sql([#card_query_param{id = CardKey, type = month = Type, key = ParamKey}|Rest], RequiredParamCardKeys, Json, Sql) ->
    IsRequired = lists:member(CardKey, RequiredParamCardKeys),
    case maps:get(ParamKey, Json, undefined) of
        undefined when IsRequired ->
            erlang:throw({error, {required_param_missing, ParamKey}});
        undefined ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql);
        YyyyMm when is_integer(YyyyMm) ->
            YyyyMmStr = integer_to_binary(YyyyMm),
            Sql1 = binary:replace(Sql, <<"{", ParamKey/binary, "}">>, YyyyMmStr, [global]),
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql1);
        _ ->
            erlang:throw({error, {param_type_mismatch, Type}})
    end;

%% Date

interpolate_sql([#card_query_param{id = CardKey, type = date = Type, key = ParamKey}|Rest], RequiredParamCardKeys, Json, Sql) ->
    IsRequired = lists:member(CardKey, RequiredParamCardKeys),
    case maps:get(ParamKey, Json, undefined) of
        undefined when IsRequired ->
            erlang:throw({error, {required_param_missing, ParamKey}});
        undefined ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql);
        B when is_binary(B) ->
            {D, T} = web_types:datetime_from_text(B),
            Date = <<"'", (format_date({D, T}))/binary, "'">>,
            Sql1 = binary:replace(Sql, <<"{", ParamKey/binary, "}">>, Date, [global]),
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql1);
        _ ->
            erlang:throw({error, {param_type_mismatch, Type}})
    end;

%% Date range

interpolate_sql([#card_query_param{id = CardKey, type = date_range = Type, key = ParamKey}|Rest], RequiredParamCardKeys, Json, Sql) ->
    IsRequired = lists:member(CardKey, RequiredParamCardKeys),
    case maps:get(ParamKey, Json, undefined) of
        undefined when IsRequired ->
            erlang:throw({error, {required_param_missing, ParamKey}});
        undefined ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql);
        #{<<"from">> := F, <<"to">> := T} when is_binary(F), is_binary(T) ->
            {FD, FT} = web_types:datetime_from_text(F),
            {TD, TT} = web_types:datetime_from_text(T),
            From = <<"'", (format_date({FD, FT}))/binary, "'">>,
            To = <<"'", (format_date({TD, TT}))/binary, "'">>,
            Sql1 = binary:replace(Sql, <<"{", ParamKey/binary, ".from}">>, From, [global]),
            Sql2 = binary:replace(Sql1, <<"{", ParamKey/binary, ".to}">>, To, [global]),
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql2);
        _ ->
            erlang:throw({error, {param_type_mismatch, Type}})
    end;

%% Time partition

interpolate_sql([#card_query_param{id = CardKey, type = time_partition = Type, key = ParamKey}|Rest], RequiredParamCardKeys, Json, Sql) ->
    IsRequired = lists:member(CardKey, RequiredParamCardKeys),
    case maps:get(ParamKey, Json, undefined) of
        undefined when IsRequired ->
            erlang:throw({error, {required_param_missing, ParamKey}});
        undefined ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql);
        Partition when is_binary(Partition) ->
            Sql1 = binary:replace(Sql, <<"{", ParamKey/binary, "}">>, Partition, [global]),
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql1);
        _ -> erlang:throw({error, {param_type_mismatch, Type}})
    end;

%% Select

interpolate_sql([#card_query_param{id = CardKey, type = select = Type, key = ParamKey, value_type = ValueType}|Rest], RequiredParamCardKeys, Json, Sql) ->
    IsRequired = lists:member(CardKey, RequiredParamCardKeys),
    case maps:get(ParamKey, Json, undefined) of
        undefined when IsRequired ->
            erlang:throw({error, {required_param_missing, ParamKey}});
        undefined ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql);
        Bool when is_boolean(Bool), ValueType =:= bool ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, binary:replace(Sql, <<"{", ParamKey/binary, "}">>, util_binary:to_binary(Bool), [global]));
        Number when is_number(Number), ValueType =:= number ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, binary:replace(Sql, <<"{", ParamKey/binary, "}">>, util_binary:to_binary(Number), [global]));
        String when is_binary(String), ValueType =:= string ->
            interpolate_sql(Rest, RequiredParamCardKeys, Json, binary:replace(Sql, <<"{", ParamKey/binary, "}">>, <<"'", String/binary, "'">>, [global]));
        _ -> erlang:throw({error, {param_type_mismatch, Type}})
    end;

%% Ignore others

interpolate_sql([_|Rest], RequiredParamCardKeys, Json, Sql) ->
    interpolate_sql(Rest, RequiredParamCardKeys, Json, Sql).

%% Format datetime

format_date({Date, {H, M, S}}) ->
    B1 = iso8601:format_date(Date),
    B2 = iso8601:format_time({H, M, trunc(S)}),
    <<B1/binary, " ", B2/binary>>.

%% Sanitize SQL query

sanitize(Sql) ->
    RestrictedKeywords = [<<"truncate ">>, <<"drop ">>, <<"detach ">>],
    Sql1 = util_binary:to_lower(Sql),
    nomatch = binary:match(Sql1, RestrictedKeywords, []).

%% Has uninterpolated param

has_uninterpolated_param(Sql) ->
    re:run(Sql, <<"\\{[a-zA-Z0-9._]+\\}">>, [global]) =/= nomatch.

%% Get optional blocks

get_optional_blocks(Sql) ->
    OpenTags = binary:matches(Sql, [<<"{?">>], []),
    CloseTags = binary:matches(Sql, [<<"?}">>], []),
    Zipped = lists:zip(OpenTags, CloseTags),
    [binary:part(Sql, From, To + ToCount - From) || {{From, _FromCount}, {To, ToCount}} <- Zipped].

%% Strip optional blocks

strip_optional_blocks(Sql) ->
    OptionalBlocks = get_optional_blocks(Sql),
    UninterpolatedBlocks = [B || B <- OptionalBlocks, has_uninterpolated_param(B)],
    Sql1 = lists:foldl(fun(B, Acc) -> binary:replace(Acc, B, <<>>, [global]) end, Sql, UninterpolatedBlocks),
    binary:replace(Sql1, [<<"{?">>, <<"?}">>],  <<>>, [global]).
