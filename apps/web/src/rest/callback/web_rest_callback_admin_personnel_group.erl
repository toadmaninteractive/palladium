-module(web_rest_callback_admin_personnel_group).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("data_protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_personnel_group/1,
    get_personnel_group_by_name/1,
    get_personnel_groups/5
]).

%% API

-spec get_personnel_group(Id) -> Response when
    Id :: integer(),
    Response :: web_protocol:personnel_group().

get_personnel_group(Id) ->
    {ok, PersonnelGroup} = db_if_personnel_groups:get_one(Id),
    web_protocol:personnel_group_from_json(PersonnelGroup).

-spec get_personnel_group_by_name(Name) -> Response when
    Name :: binary(),
    Response :: web_protocol:personnel_group().

get_personnel_group_by_name(Name) ->
    {ok, PersonnelGroup} = db_if_personnel_groups:get_one_by_name(Name),
    web_protocol:personnel_group_from_json(PersonnelGroup).

-spec get_personnel_groups(Needle, OrderBy, OrderDir, Offset, Limit) -> Response when
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:personnel_group_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Response :: data_protocol:collection_slice(web_protocol:personnel_group()).

get_personnel_groups(Needle, OrderBy, OrderDir, Offset, Limit) ->
    {ok, PersonnelGroups} = db_if_personnel_groups:get(Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_groups:get_count(Needle),
    PersonnelGroups1 = lists:map(fun web_protocol:personnel_group_from_json/1, PersonnelGroups),
    #collection_slice{items = PersonnelGroups1, total = Total}.

%% Local functions
