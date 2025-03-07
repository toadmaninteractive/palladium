-module(web_rest_callback_projects).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("acl.hrl").
-include("session.hrl").
-include("settings.hrl").
-include("card_protocol.hrl").
-include("data_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    get_project/2,
    get_projects/1
]).

%% API

-spec get_project(ProjectId, Req) -> Response when
    ProjectId :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:project_config(), cowboy_req:req()}.

get_project(ProjectId, #{?m_session := #session{key = {?actor_personnel, _SessionId}, user_id = UserId}} = Req) ->
    Db = web_analytics:project_db(ProjectId),
    AccessibleDbs = web_acl:accessible_dbs(ProjectId, UserId),
    {#project_config{id = ProjectId, name = card_config:project_name(Db), databases = AccessibleDbs}, Req}.

-spec get_projects(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {data_protocol:collection(web_protocol:project_config()), cowboy_req:req()}.

get_projects(#{?m_session := #session{key = {?actor_personnel, _SessionId}, user_id = UserId}} = Req) ->
    {ok, AvailableIds} = db_if_projects:get_all_ids(),
    AvailableIdSet = gb_sets:from_list(AvailableIds),
    AvailableEcouchDbs = [Db || Db <- ecouch:dbs(), gb_sets:is_element(atom_to_binary(Db, utf8), AvailableIdSet)],
    Projects = [begin
        Project = atom_to_binary(Db, utf8),
        AccessibleDbs = web_acl:accessible_dbs(Project, UserId),
        #project_config{id = Project, name = card_config:project_name(Db), databases = AccessibleDbs}
    end || Db <- AvailableEcouchDbs],
    Projects1 = [P || #project_config{databases = Dbs} = P <- Projects, Dbs =/= []],
    {#collection{items = Projects1}, Req}.

%% Local functions
