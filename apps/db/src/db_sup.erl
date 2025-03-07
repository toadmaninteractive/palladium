-module(db_sup).

-behaviour(supervisor).

%% Include files

-include("db.hrl").

%% Exported functions

-export([
    start_link/0
]).

%% supervisor callbacks

-export([
    init/1
]).

%% API

-spec start_link() -> supervisor:startlink_ret().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks

init(_Args) ->
    Pools = [{N, maps:from_list(PL)} || {N, PL} <- db_config:pools()],
    PoolSpecs = lists:filtermap(fun
        % PostgreSQL pool
        ({PoolName, #{type := postgres, pool_size := Size, pool_max_overflow := MaxOverflow} = Props}) ->
            WorkerArgs = maps:to_list(maps:with([hostname, database, username, password], Props)),
            PoolArgs = maps:to_list(#{name => {local, PoolName}, worker_module => postgres_worker, size => Size, max_overflow => MaxOverflow}),
            {true, poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)}
    end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

%% Local functions
