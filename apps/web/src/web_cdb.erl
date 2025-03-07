-module(web_cdb).

-behaviour(gen_server).

%% Include files

%% Exported functions

-export([
    start_link/0
]).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(init_msg, init).
-define(init_interval, 0).              % Instantly
-define(refresh_msg, refresh).
-define(refresh_interval, 60 * 1000).   % 1 min

-record(state, {}).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

init(_Args) ->
    erlang:send_after(?init_interval, self(), ?init_msg),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p~n", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(?init_msg, State) ->
    refresh(),
    catalogue:subscribe(),
    erlang:send_after(?refresh_interval, self(), ?refresh_msg),
    {noreply, State};

handle_info(?refresh_msg, State) ->
    refresh(),
    erlang:send_after(?refresh_interval, self(), ?refresh_msg),
    {noreply, State};

handle_info({catalogue, Db, config}, State) ->
    update_project(Db),
    logger:info("Project updated: <~s>", [Db], #{caption => ?MODULE}),
    {noreply, State};

handle_info({catalogue, _Db, _Key}, State) ->
    {noreply, State};

handle_info({catalogue, _Key}, State) ->
    {noreply, State};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

refresh() ->
    SyncTs = time:milliseconds(),
    [update_project(Db, SyncTs) || Db <- ecouch:dbs()],
    db_if_projects:disable_sync(SyncTs).

update_project(Db) ->
    SyncTs = time:milliseconds(),
    update_project(Db, SyncTs).

update_project(Db, SyncTs) ->
    try
        ProjectId = atom_to_binary(Db, utf8),
        Title = card_config:project_name(Db),
        Databases = card_config:databases(Db),
        {ok, ProjectId} = db_if_projects:create(ProjectId, Title, Databases, SyncTs)
    catch Type:What:StackTrace ->
        logger:error("Failed to update project <~s> (reason: ~p:~p)", [Db, Type, What], #{caption => ?MODULE, stacktrace => StackTrace})
    end.
