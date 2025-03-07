-module(web_rest_callback_admin_personnel).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("data_protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_personnel_account/1,
    get_personnel_account_by_username/1,
    get_personnel_accounts/5
]).

%% API

-spec get_personnel_account(Id) -> Response when
    Id :: integer(),
    Response :: web_protocol:personnel_account().

get_personnel_account(Id) ->
    {ok, PersonnelAccount} = db_if_personnel:get_one(Id),
    web_protocol:personnel_account_from_json(PersonnelAccount).

-spec get_personnel_account_by_username(Username) -> Response when
    Username :: binary(),
    Response :: web_protocol:personnel_account().

get_personnel_account_by_username(Username) ->
    {ok, PersonnelAccount} = db_if_personnel:get_one_by_username(Username),
    web_protocol:personnel_account_from_json(PersonnelAccount).

-spec get_personnel_accounts(Needle, OrderBy, OrderDir, Offset, Limit) -> Response when
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:personnel_account_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Response :: data_protocol:collection_slice(web_protocol:personnel_account()).

get_personnel_accounts(Needle, OrderBy, OrderDir, Offset, Limit) ->
    {ok, PersonnelAccounts} = db_if_personnel:get(Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel:get_count(Needle),
    PersonnelAccounts1 = lists:map(fun web_protocol:personnel_account_from_json/1, PersonnelAccounts),
    #collection_slice{items = PersonnelAccounts1, total = Total}.

%% Local functions
