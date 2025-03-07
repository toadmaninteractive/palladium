-module(clickhouse_config).

%% Include files

%% Exported functions

-export([
    url/1,
    username/1,
    password/1
]).

-define(clickhouse_app, clickhouse).

%% API

-spec url(Server :: atom()) -> string().

url(Server) ->
    group_param(Server, url).

-spec username(Server :: atom()) -> string().

username(Server) ->
    group_param(Server, username).

-spec password(Server :: atom()) -> string().

password(Server) ->
    group_param(Server, password).

%% Local functions

group_param(GroupName, ParamName) ->
    {ok, Group} = application:get_env(?clickhouse_app, GroupName),
    proplists:get_value(ParamName, Group).
