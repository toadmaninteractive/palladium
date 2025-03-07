-module(ch_query).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("db.hrl").

%% Exported functions

-export([
    sql_query/1,
    sql_query/2
]).

-export_type([
    statement/0
]).

-type statement() :: binary() | string().
-type result_tuple() :: {'selected', ColNames :: any(), Rows :: any()} | string() | {'updated', NRows :: non_neg_integer()}.

%% API

-spec sql_query(Statement :: statement()) ->
    result_tuple() | [result_tuple()] | {'error', Reason :: atom()}.

sql_query(Statement) ->
    sql_query(?pool_clickhouse, Statement).

-spec sql_query(PoolName :: atom(), Statement :: statement()) ->
    result_tuple() | [result_tuple()] | {'error', Reason :: atom()}.

sql_query(PoolName, Statement) ->
    Result = poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {sql_query, Statement})
    end),
    log_error(Result),
    Result.

%% Local functions

log_error({error, Reason}) ->
    logger:error("ClickHouse query failed: ~p", [Reason]),
    {error, Reason};
log_error(_) ->
    ignore.
