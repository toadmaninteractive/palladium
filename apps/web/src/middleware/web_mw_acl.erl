-module(web_mw_acl).

-behaviour(cowboy_middleware).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("acl.hrl").
-include("http.hrl").
-include("session.hrl").

%% Exported functions

-export([
    execute/2
]).

%% API

execute(#{?m_session := #session{key = {personnel, _}, user_id = PersonnelId}} = Req, #{handler_opts := #acl{} = ACL} = Env) ->
    Method = cowboy_req:method(Req),
    Bindings = cowboy_req:bindings(Req),
    case is_allowed(Method, Bindings, PersonnelId, ACL) of
        true -> {ok, Req, Env};
        false -> {stop, forbidden(Req)}
    end;

execute(Req, #{handler_opts := #acl{}}) ->
    {stop, forbidden(Req)};

execute(Req, Env) ->
    {ok, Req, Env}.

%% Local functions

forbidden(Req) ->
    cowboy_req:reply(?http_forbidden, #{}, Req).

method_role(?get, #acl{get = Role}) -> Role;
method_role(?post, #acl{post = Role}) -> Role;
method_role(?put, #acl{put = Role}) -> Role;
method_role(?patch, #acl{patch = Role}) -> Role;
method_role(?delete, #acl{delete = Role}) -> Role;
method_role(_, _) -> undefined.

is_superadmin(PersonnelId) ->
    case db_if_personnel:is_superadmin(PersonnelId) of
        {ok, true} -> true;
        _ -> false
    end.

is_project_accessible(ProjectId, PersonnelId) ->
    case db_if_projects:is_accessible(ProjectId, PersonnelId) of
        {ok, true} -> true;
        _ -> false
    end.

project_and_db(Bindings, #acl{project = P, database = D}) when P =/= undefined, D =/= undefined ->
    {maps:get(P, Bindings, undefined), maps:get(D, Bindings, undefined)};
project_and_db(Bindings, #acl{project = P}) when P =/= undefined ->
    {maps:get(P, Bindings, undefined), undefined};
project_and_db(Bindings, #acl{database = D}) when D =/= undefined ->
    Database = maps:get(D, Bindings, undefined),
    Pairs = [{atom_to_binary(Db, utf8), card_config:databases(Db)} || Db <- ecouch:dbs()],
    case [P || {P, Databases} <- Pairs, lists:member(Database, Databases)] of
        [Project | _] -> {Project, Database};
        _ -> {undefined, undefined}
    end;
project_and_db(_, _) -> {undefined, undefined}.

is_allowed(Method, Bindings, PersonnelId, ACL) ->
    % Check for local admin option
    IsLocalAdmin = access_config:local_admin(),

    % Check if current user is superadmin
    IsSuperadmin = is_superadmin(PersonnelId),

    % Get project and database
    {ProjectId, Database} = project_and_db(Bindings, ACL),

    % Proceed with role check
    case method_role(Method, ACL) of
        % No role check required
        undefined -> true;

        % Superadmin access required
        ?role_superadmin -> IsSuperadmin orelse IsLocalAdmin;

        % Superadmin is allowed to access everyting else
        _ when IsSuperadmin -> true;

        % Local admin is also allowed to access everyting else
        _ when IsLocalAdmin -> true;

        % Consumer
        ?role_consumer ->
            if
                is_binary(ProjectId), is_binary(Database) ->
                    AccessibleDbs = web_acl:accessible_dbs(ProjectId, PersonnelId),
                    lists:member(Database, AccessibleDbs);
                is_binary(ProjectId) ->
                    is_project_accessible(ProjectId, PersonnelId);
                true -> false
            end;

        % Everyone else
        MinimalAcceptableRole ->
            case db_if_personnel_roles:get_one(PersonnelId, ProjectId) of
                {ok, #{<<"user_role">> := UserRole, <<"group_roles">> := GroupRoles}} ->
                   Roles = [web_protocol:access_role_from_json(R) || R <- [UserRole | maps:values(GroupRoles)], is_binary(R)],
                   RoleLevels = [web_acl:role_level(R) || R <- lists:usort(Roles)],
                   MaxLevel = ?yesno(RoleLevels =:= [], 0, lists:max(RoleLevels)),
                   MaxLevel >= web_acl:role_level(MinimalAcceptableRole);
                _ -> false
            end
    end.
