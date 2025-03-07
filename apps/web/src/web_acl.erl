-module(web_acl).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("acl.hrl").
-include("card_protocol.hrl").

%% Exported functions

-export([
    role_level/1,
    accessible_dbs/2
]).

%% API

-spec role_level(Role) -> Result when
    Role :: ?role_consumer | ?role_maintainer | ?role_admin | ?role_superadmin | any(),
    Result :: non_neg_integer().

role_level(?role_consumer) -> 1;
role_level(?role_maintainer) -> 2;
role_level(?role_admin) -> 3;
role_level(_) -> 0.

-spec accessible_dbs(ProjectId, PersonnelId) -> Result when
    ProjectId :: binary(),
    PersonnelId :: non_neg_integer(),
    Result :: [binary()].

accessible_dbs(ProjectId, PersonnelId) ->
    IsLocalAdmin = access_config:local_admin(),
    {ok, IsSuperAdmin} = db_if_personnel:is_superadmin(PersonnelId),
    {ok, ACL} = db_if_personnel_roles:get_one(PersonnelId, ProjectId),
    #{
        <<"project_dbs">> := ProjectDbs,
        <<"user_role">> := UserRole,
        <<"is_global">> := IsUserGlobal,
        <<"dbs">> := UserDbs,
        <<"group_roles">> := GroupRoleMap % map: GroupName => #{dbs, is_global, role}
    } = ACL,
    GroupRoles = maps:values(GroupRoleMap),
    IsUserGlobal1 = IsUserGlobal =:= true,
    UserRole1 = ?yesno(is_binary(UserRole), web_protocol:access_role_from_json(UserRole), undefined),
    UserDbs1 = ?yesno(is_list(UserDbs), UserDbs, []),
    InheritedRoles = [web_protocol:access_role_from_json(Role) || #{<<"role">> := Role} <- GroupRoles],
    InheritedDbs = lists:flatten([Dbs || #{<<"dbs">> := Dbs} <- GroupRoles]),
    InheritedGlobal = lists:member(true, [IsGlobal || #{<<"is_global">> := IsGlobal} <- GroupRoles]),
    MaxRoleLevel = lists:max([web_acl:role_level(Role) || Role <- [UserRole1 | InheritedRoles]]),
    MinPrivilegedLevel = web_acl:role_level(?role_maintainer),
    MinPossibleLevel = web_acl:role_level(?role_consumer),
    AvailableDbSet = gb_sets:from_list(UserDbs1 ++ InheritedDbs),
    EcouchDb = binary_to_existing_atom(ProjectId, utf8),
    ActualDatabases = try
        card_config:databases(EcouchDb)
    catch _:_:_ -> ProjectDbs
    end,
    if
        % Is superadmin or local admin
        IsSuperAdmin; IsLocalAdmin -> ActualDatabases;

        % Has privileged access role
        MaxRoleLevel >= MinPrivilegedLevel -> ActualDatabases;

        % Has non-privileged access role but has global access
        MaxRoleLevel >= MinPossibleLevel, (IsUserGlobal1 orelse InheritedGlobal) -> ActualDatabases;

        % Has non-privileged access role without global access
        MaxRoleLevel >= MinPossibleLevel -> [Db || Db <- ActualDatabases, gb_sets:is_element(Db, AvailableDbSet)];

        % No access
        true -> []
    end.

%% Local functions
