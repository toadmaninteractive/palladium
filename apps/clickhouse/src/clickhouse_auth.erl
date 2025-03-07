-module(clickhouse_auth).

%% Include files

%% Exported functions

-export([
    basic/2
]).

%% API

-spec basic(Username, Password) -> Result when
    Username :: string(),
    Password :: string(),
    Result :: string().

basic(Username, Password) ->
    EncodedCredentials = base64:encode_to_string(Username ++ ":" ++ Password),
    "Basic " ++ EncodedCredentials.

%% Local functions
