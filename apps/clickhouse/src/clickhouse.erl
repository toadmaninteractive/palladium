-module(clickhouse).

%% Include files

-include("clickhouse_protocol.hrl").

%% Exported functions

-export([
    default_url/0,
    default_auth/0
]).

%% API

-spec default_url() -> string().

default_url() ->
    clickhouse_config:url(default).

-spec default_auth() -> string().

default_auth() ->
    Username = clickhouse_config:username(default),
    Password = clickhouse_config:password(default),
    clickhouse_auth:basic(Username, Password).

%% Local functions
