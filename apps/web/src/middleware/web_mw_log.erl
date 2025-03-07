-module(web_mw_log).

-behaviour(cowboy_middleware).

%% Include files

%% Exported functions

-export([
    execute/2
]).

%% API

execute(Req, Env) ->
    % Disable output
    % logger:debug("~s ~s~n", [cowboy_req:method(Req), cowboy_req:url(Req)], #{caption => ?MODULE}),
    {ok, Req, Env}.

%% Local functions
