-module(web_rest_callback_parameters).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("session.hrl").
-include("settings.hrl").
-include("card_protocol.hrl").
-include("data_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    retrieve_parameter_values/4
]).

%% API

-spec retrieve_parameter_values(Request, ProjectId, Database, ParamKey) -> Result when
    Request :: igor_json:json(),
    ProjectId :: binary(),
    Database :: binary(),
    ParamKey :: binary(),
    Result :: data_protocol:collection(data_protocol:json_point()).

retrieve_parameter_values(Request, ProjectId, Database, ParamKey) ->
    Db = web_analytics:project_db(ProjectId),
    ClickhouseServer = card_config:clickhouse_server(Db),
    Cards = [cards:card_query_param(Db, QPK) || QPK <- card_config:parameters(Db)],
    Items = case [Card || #card_query_param{key = Key} = Card <- Cards, Key =:= ParamKey] of
        [#card_query_param{input_sql = Sql, depends_on = DependsOnCardKeys} | _] when is_binary(Sql) ->
            DependsOnCardKeys1 = ?yesno(is_list(DependsOnCardKeys), DependsOnCardKeys, []),
            Dependencies = [cards:card_query_param(Db, DPK) || DPK <- DependsOnCardKeys1],
            web_analytics:dynamic_values(ClickhouseServer, Database, Sql, Dependencies, Request);
        _ -> []
    end,
    #collection{items = Items}.

%% Local functions
