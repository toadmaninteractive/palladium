-module(db_if_projects).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get/6,
    get_count/2,
    get_all/0,
    get_all_ids/0,
    get_accessible/1,
    create/4,
    update/2,
    update/3,
    delete/1,
    undelete/1,
    enable/1,
    disable/1,
    disable_sync/1,
    dbs/1,
    exists/1,
    is_accessible/2
]).

%% API

-spec get_one(ProjectId :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(ProjectId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM projects AS proj ",
        (common_joins())/binary,
        "WHERE proj.id = TRIM($1)"
    >>,
    case db_query:select_one(Query, [ProjectId]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(PersonnelId, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    PersonnelId :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(), Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(PersonnelId, Needle, OrderBy, OrderDir, Offset, Limit) ->
    Role = consumer,
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<"AND (strpos(LOWER(proj.id), LOWER($3)) > 0 OR strpos(LOWER(proj.title), LOWER($3)) > 0)">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM projects AS proj ",
        (common_joins())/binary,
        "WHERE is_project_access_role_sufficient(proj.id, $1, $2) ",
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = [PersonnelId, Role] ++ ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_count(PersonnelId, Needle) -> Result when
    PersonnelId :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(PersonnelId, Needle) ->
    Role = consumer,
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<"AND (strpos(LOWER(proj.id), LOWER($3)) > 0 OR strpos(LOWER(proj.title), LOWER($3)) > 0)">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM projects AS proj ",
        "WHERE is_project_access_role_sufficient(proj.id, $1, $2) ",
        NeedlePart/binary
    >>,
    Params = [PersonnelId, Role] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all() ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_all() ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM projects AS proj ",
        (common_joins())/binary,
        "ORDER BY title ASC"
    >>,
    db_query:select(Query, []).

-spec get_all_ids() ->
    {'ok', Items :: [binary()]} | {'error', Reason :: atom()}.

get_all_ids() ->
    Query = <<"SELECT id FROM projects">>,
    case db_query:select(Query, []) of
        {ok, Items} -> {ok, [Id || #{<<"id">> := Id} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_accessible(PersonnelId :: non_neg_integer()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_accessible(PersonnelId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM projects AS proj ",
        (common_joins())/binary,
        "WHERE is_project_accessible_by_personnel(proj.id, $1) ",
        "ORDER BY title ASC"
    >>,
    db_query:select(Query, [PersonnelId]).

-spec create(ProjectId :: binary(), Title :: binary(), Dbs :: non_neg_integer(), SyncTs :: binary()) ->
    {'ok', ProjectId :: binary()} | {'error', Reason :: atom()}.

create(ProjectId, Title, Dbs, SyncTs) ->
    Query = <<
        "INSERT INTO projects (id, title, dbs, sync_ts) ",
        "VALUES (TRIM($1), TRIM($2), $3, $4) ",
        "ON CONFLICT (id) DO UPDATE SET ",
            "title = TRIM($2), ",
            "dbs = $3, ",
            "sync_ts = $4, ",
            "is_disabled = FALSE, ",
            "is_deleted = FALSE, ",
            "updated_at = current_timestamp ",
        "RETURNING id::varchar"
    >>,
    Params = [ProjectId, Title, jsx:encode(Dbs), SyncTs],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := ProjectId}] = db_util:result_to_json(Columns, Rows),
            {ok, ProjectId};
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"projects_pkey">> -> {error, project_id_already_exists};
                <<"proj_title_ult_index">> -> {error, project_title_already_exists}
            end;
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"projects_id_check">> -> {error, invalid_project_id};
                <<"projects_title_check">> -> {error, invalid_project_title}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec update(ProjectId :: binary(), Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(ProjectId, Patch) ->
    update(ProjectId, undefined, Patch).

-spec update(ProjectId :: binary(), Rev :: non_neg_integer() | 'undefined', Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(ProjectId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_trim(title),
        dbs,
        sync_ts,
        is_disabled,
        is_deleted
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"projects">>, <<"id">>, ProjectId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, not_exists};
                {error, #error{codename = unique_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"proj_title_ult_index">> -> {error, project_title_already_exists}
                    end;
                {error, #error{codename = check_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"projects_title_check">> -> {error, invalid_project_title}
                    end;
                {error, #error{codename = Code}} -> {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(ProjectId :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

delete(ProjectId) ->
    update(ProjectId, #{<<"is_deleted">> => true}).

-spec undelete(ProjectId :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

undelete(ProjectId) ->
    update(ProjectId, #{<<"is_deleted">> => false}).

-spec enable(ProjectId :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

enable(ProjectId) ->
    update(ProjectId, #{<<"is_disabled">> => false}).

-spec disable(ProjectId :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

disable(ProjectId) ->
    update(ProjectId, #{<<"is_disabled">> => true}).

-spec disable_sync(LastSyncTs :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

disable_sync(LastSyncTs) ->
    Query = <<"UPDATE projects SET is_disabled = TRUE WHERE sync_ts <> $1">>,
    case db_query:update(Query, [LastSyncTs], [raw_error]) of
        {ok, _NumDisabled} -> ok;
        {error, #error{codename = Code}} -> {error, Code}
    end.

-spec dbs(ProjectId :: binary()) ->
    {'ok', [binary()]} | {'error', Reason :: atom()}.

dbs(ProjectId) ->
    Query = <<"SELECT dbs FROM projects WHERE id = TRIM($1)">>,
    case db_query:select_one(Query, [ProjectId]) of
        {ok, #{<<"dbs">> := Dbs}} -> {ok, Dbs};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec exists(ProjectId :: binary()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(ProjectId) ->
    Query = <<"SELECT COUNT(*)::bigint AS count FROM projects WHERE id = TRIM($1)">>,
    case db_query:select_one(Query, [ProjectId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec is_accessible(ProjectId, PersonnelId) -> Result when
    ProjectId :: binary(),
    PersonnelId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

is_accessible(ProjectId, PersonnelId) ->
    Query = <<"SELECT is_project_accessible_by_personnel($1, $2) AS result">>,
    case db_query:select_one(Query, [ProjectId, PersonnelId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % proj : projects
    <<
        "proj.id AS id, ",
        "proj.rev AS rev, ",
        "proj.title AS title, ",
        "proj.dbs AS dbs, ",
        "proj.sync_ts AS sync_ts, ",
        "proj.is_disabled AS is_disabled, ",
        "proj.is_deleted AS is_deleted, ",
        "proj.created_at AS created_at, ",
        "proj.updated_at AS updated_at "
    >>.

common_joins() ->
    <<>>.
