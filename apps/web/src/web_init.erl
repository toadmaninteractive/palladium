-module(web_init).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("acl.hrl").
-include("settings.hrl").

%% Exported functions

-export([
    start_cowboy/4
]).

%% API

start_cowboy(BindIp, BindPort, Acceptors, Secure) ->
    % Get paths, make up index and favicon
    WebDir = filename:join([filename:absname(""), "web"]),
    SslDir = filename:join([WebDir, "ssl"]),
    FrontendDir = filename:join([WebDir, "frontend"]),
    FrontendDistDir = filename:join([FrontendDir, "dist"]),
    Index = "index.html",
    FavIcon = "favicon.ico",

    % Routes
    Routes = [
        % REST API: Administration
        {"/api/admin/personnel/:id", web_rest_admin_personnel, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel/username/:username", web_rest_admin_personnel_username, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel/:id/roles", web_rest_admin_personnel_roles, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel/:id/roles/:project", web_rest_admin_personnel_role_set, #acl{put = ?role_superadmin, delete = ?role_superadmin}},
        {"/api/admin/personnels", web_rest_admin_personnels, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/:id", web_rest_admin_personnel_group, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/name/:name", web_rest_admin_personnel_group_name, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/:id/roles", web_rest_admin_personnel_group_roles, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/:id/roles/:project", web_rest_admin_personnel_group_role_set, #acl{put = ?role_superadmin, delete = ?role_superadmin}},
        {"/api/admin/personnel-groups", web_rest_admin_personnel_groups, #acl{get = ?role_superadmin}},
        {"/api/admin/settings", web_rest_admin_settings, #acl{get = ?role_superadmin, put = ?role_superadmin}},

        % REST API: Analytics
        {"/api/analytics/data/:project", web_rest_analytics_data, #acl{get = ?role_consumer, project = project}},
        {"/api/analytics/query/:project/:database/:query", web_rest_analytics_query, #acl{put = ?role_consumer, project = project, database = database}},

        % REST API: Authentication and authorization for personnel
        {"/api/auth/personnel/login", web_rest_auth_personnel_login, []},
        {"/api/auth/personnel/logout", web_rest_auth_personnel_logout, []},
        {"/api/auth/personnel/profile", web_rest_auth_personnel_profile, []},
        {"/api/auth/personnel/status", web_rest_auth_personnel_status, []},

        % REST API: Dashboard
        {"/api/dashboard/data/:project", web_rest_dashboard_data, #acl{get = ?role_consumer, project = project}},
        {"/api/dashboard/:database/asl", web_rest_dashboard_asl, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/dau", web_rest_dashboard_dau, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/daup", web_rest_dashboard_daup, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/mau", web_rest_dashboard_mau, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/nu", web_rest_dashboard_nu, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/nud", web_rest_dashboard_nud, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/nut", web_rest_dashboard_nut, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/pau", web_rest_dashboard_pau, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/pasl", web_rest_dashboard_pasl, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/pnu", web_rest_dashboard_pnu, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/ptuu", web_rest_dashboard_ptuu, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/sdd", web_rest_dashboard_sdd, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/tuu", web_rest_dashboard_tuu, #acl{get = ?role_consumer, database = database}},
        {"/api/dashboard/:database/uubp", web_rest_dashboard_uubp, #acl{get = ?role_consumer, database = database}},

        % REST API: Miscellaneous
        {"/api/events/:project", web_rest_events, #acl{get = ?role_consumer, project = project}},
        {"/api/patch-notes/:project", web_rest_patch_notes, #acl{get = ?role_consumer, project = project}},
        {"/api/parameters/values/:project/:database/:param", web_rest_param_values, #acl{put = ?role_consumer, project = project, database = database}},
        {"/api/projects/:project", web_rest_project, #acl{get = ?role_consumer, project = project}},
        {"/api/projects/:project/roles/account", web_rest_project_roles_account, #acl{get = ?role_superadmin}},
        {"/api/projects/:project/roles/group", web_rest_project_roles_group, #acl{get = ?role_superadmin}},
        {"/api/projects/:project/roles/me", web_rest_project_roles_me, []},
        {"/api/projects", web_rest_projects, []},
        {"/api/widgets/query/:project/:database/:query", web_rest_widget_query, #acl{put = ?role_consumer, project = project, database = database}},

        % Dashboard
        {"/assets/[...]", cowboy_static, {dir, filename:join([FrontendDistDir, "assets"])}},
        {"/static/[...]", cowboy_static, {dir, filename:join([FrontendDistDir, "static"])}},
        {"/dist/[...]", cowboy_static, {dir, FrontendDistDir}},
        {"/" ++ FavIcon, cowboy_static, {file, filename:join([FrontendDistDir, FavIcon])}},
        {'_', cowboy_static, {file, filename:join([FrontendDistDir, Index])}}
    ],

    % Compile route dispatcher
    Dispatch = cowboy_router:compile([{'_', Routes}]),

    % Define middlewares
    Middlewares = [
        web_mw_log,
        web_mw_access_control,
        web_mw_no_cache,
        web_mw_authenticate,
        web_mw_authorize,
        % web_mw_websocket,
        cowboy_router,
        web_mw_acl,
        cowboy_handler
    ],

    % Define SSL options
    SslOpts = get_ssl_opt(cacertfile, SslDir) ++ get_ssl_opt(certfile, SslDir) ++ get_ssl_opt(keyfile, SslDir),

    % Define server starter, options and environment
    ServerName = iolist_to_binary(io_lib:format("~s_~s", [palladium_web, ?yesno(Secure, https, http)])),
    StarterFun = case Secure of true -> fun cowboy:start_tls/3; false -> fun cowboy:start_clear/3 end,
    Opts = [{ip, BindIp}, {port, BindPort}],
    OptsMaybeWithSsl = ?yesno(Secure, Opts ++ SslOpts, Opts),

    % Define path prefixes
    AuthenticatePrefixes = [<<"/api/">>],
    AuthorizePrefixes = [<<"/api/admin/">>, <<"/api/dashboard/">>, <<"/api/projects">>],

    % Start server
    ProtocolOpts = #{
        env => #{dispatch => Dispatch, authenticate_prefixes => AuthenticatePrefixes, authorize_prefixes => AuthorizePrefixes},
        middlewares => Middlewares
    },

    StarterFun(ServerName, OptsMaybeWithSsl, ProtocolOpts).

%% Local functions

absolute_or_local(FilePath, LocalSslDir) ->
    case filelib:is_regular(FilePath) of
        true -> FilePath;
        false -> filename:join([LocalSslDir, FilePath])
    end.

get_ssl_opt(Param, LocalSslDir) ->
    Result = case Param of
        cacertfile -> web_config:cacertfile();
        certfile -> web_config:certfile();
        keyfile -> web_config:keyfile();
        _ -> undefined
    end,
    case Result of
        {ok, Value} -> [{Param, absolute_or_local(Value, LocalSslDir)}];
        undefined -> []
    end.
