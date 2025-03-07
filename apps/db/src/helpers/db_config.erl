-module(db_config).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("db.hrl").

%% Exported functions

-export([
    % Database
    auto_migrate/0,
    pools/0,

    % Migration
    migrations_config/0,
    last_version/0,
    migrations/0,
    setup_sql/0,
    patch_sql/1
]).

%% API

-spec auto_migrate() -> boolean().

auto_migrate() ->
    application:get_env(?db_app, auto_migrate, false).

-spec pools() -> [{Name :: atom(), Props :: proplists:proplist()}].

pools() ->
    application:get_env(?db_app, pools, []).

migrations_config() ->
    MigrationsFilename = filename:join([code:priv_dir(?db_app), "migrations.config"]),
    case file:consult(MigrationsFilename) of
        {ok, [PropList]} when is_list(PropList) -> {ok, PropList};
        {error, Reason} when is_atom(Reason) -> {error, Reason};
        {error, _} -> {error, badfile}
    end.

-spec last_version() ->
    {'ok', Version :: non_neg_integer()} | {'error', Reason :: 'not_exists' | 'badtype'}.

last_version() ->
    {ok, Migrations} = migrations_config(),
    case proplists:get_value(last_version, Migrations) of
        undefined -> {error, not_exists};
        Version when is_integer(Version) -> {ok, Version};
        _ -> {error, badtype}
    end.

-spec migrations() ->
    {'ok', [{Version :: non_neg_integer(), {'sql', Filename :: string()}}]} | {'error', Reason :: 'not_exists' | 'badtype'}.

migrations() ->
    {ok, Migrations} = migrations_config(),
    case proplists:get_value(migrations, Migrations) of
        undefined -> {error, not_exists};
        List when is_list(List) -> {ok, List};
        _ -> {error, badtype}
    end.

-spec setup_sql() ->
    {'ok', Statements :: binary()} | {'error', Reason :: atom()}.

setup_sql() ->
    SetupFilename = filename:join([code:priv_dir(?db_app), "sql", "setup", "setup.sql"]),
    load_sql(SetupFilename).

-spec patch_sql(Filename :: string() | binary()) ->
    {'ok', Statements :: binary()} | {'error', Reason :: atom()}.

patch_sql(Filename) ->
    PatchFilename = filename:join([code:priv_dir(?db_app), "sql", "patch", Filename]),
    load_sql(PatchFilename).

%% Local functions

load_sql(Filename) ->
    case file:read_file(Filename) of
        {ok, Statements} -> {ok, Statements};
        {error, Reason} -> {error, Reason}
    end.
