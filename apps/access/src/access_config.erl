-module(access_config).

%% Include files

%% Exported functions

-export([
    auth_realm/0,
    admin_group/0,
    local_admin/0,
    ldap_sync/0
]).

-define(access_app, access).

%% API

-spec auth_realm() -> atom().

auth_realm() ->
    {ok, AuthRealm} = application:get_env(?access_app, auth_realm),
    AuthRealm.

-spec admin_group() -> binary().

admin_group() ->
    {ok, AdminGroup} = application:get_env(?access_app, admin_group),
    util_binary:to_binary(AdminGroup).

-spec local_admin() -> boolean().

local_admin() ->
    case application:get_env(?access_app, local_admin, undefined) of
        LocalAdmin when is_boolean(LocalAdmin) -> LocalAdmin;
        _ -> false
    end.

-spec ldap_sync() -> boolean().

ldap_sync() ->
    case application:get_env(?access_app, ldap_sync, undefined) of
        LdapSync when is_boolean(LdapSync) -> LdapSync;
        _ -> true
    end.

%% Local functions
