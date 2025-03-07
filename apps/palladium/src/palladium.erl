-module(palladium).

%% Include files

%% Exported functions

-export([
    start/0
]).

%% API

-spec start() -> 'ok' | {'error', Reason::term()}.

start() ->
    aplib:start_app_recursive(palladium).

%% Local functions
