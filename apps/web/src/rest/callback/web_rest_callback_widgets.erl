-module(web_rest_callback_widgets).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("session.hrl").
-include("card_protocol.hrl").
-include("data_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    send_widget_request/4
]).

%% API

-spec send_widget_request(Request, ProjectId, Database, Query) -> Result when
    Request :: igor_json:json(),
    ProjectId :: binary(),
    Database :: binary(),
    Query :: binary(),
    Result :: web_protocol:query_result().

send_widget_request(Request, ProjectId, Database, Query) ->
    ClickhouseServer = web_analytics:project_clickhouse_server(ProjectId),
    Db = web_analytics:project_db(ProjectId),
    Widgets = [cards:card_widget(Db, Key) || Key <- card_config:dashboard_widgets(Db)],
    [Widget | _] = [W || #card_widget{key = QueryKey} = W <- Widgets, QueryKey =:= Query],
    Params = [cards:card_query_param(Db, Key) || Key <- Widget#card_widget.params],
    #card_widget{query = #query_node{input_sql = Sql, output = Output}} = Widget,
    Node = #analytics_query_node_result{result = web_analytics:send_query(ClickhouseServer, Database, Request, [], Params, Widget#card_widget.params, Sql, Output)},
    #widget_query_result{query = Query, result = Node}.

%% Local functions
