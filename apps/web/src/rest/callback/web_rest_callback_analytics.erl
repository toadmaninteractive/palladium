-module(web_rest_callback_analytics).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("card_protocol.hrl").
-include("data_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    get_analytics_data/1,
    send_analytics_request/4
]).

%% API

-spec get_analytics_data(ProjectId :: binary()) ->
    web_protocol:analytics_data().

get_analytics_data(ProjectId) ->
    web_analytics:project_data(ProjectId).

-spec send_analytics_request(Request, ProjectId, Database, Query) -> Result when
    Request :: igor_json:json(),
    ProjectId :: binary(),
    Database :: binary(),
    Query :: binary(),
    Result :: web_protocol:query_result().

send_analytics_request(Request, ProjectId, Database, Query) ->
    ClickhouseServer = web_analytics:project_clickhouse_server(ProjectId),
    #card_query{nodes = Nodes, required_params = RequiredParamCardKeys} = web_analytics:project_query(ProjectId, Query),
    Params = web_analytics:project_query_params(ProjectId, Query),
    NodeSeries = [begin
        #analytics_query_node_result{result = web_analytics:send_query(ClickhouseServer, Database, Request, [], Params, RequiredParamCardKeys, Sql, Output)}
    end || #query_node{input_sql = Sql, output = Output} <- Nodes],
    #analytics_query_result{query = Query, result = NodeSeries}.

%% Local functions
