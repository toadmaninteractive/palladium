-module(web_rest_callback_events).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("session.hrl").
-include("settings.hrl").
-include("card_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    get_events/1
]).

%% API

-spec get_events(ProjectId :: binary()) ->
    web_protocol:event_data().

get_events(ProjectId) ->
    Db = web_analytics:project_db(ProjectId),
    #card_config{event_super_fields = EventSuperFields, event_groups = EventGroupCardKeys} = cards:card_config(Db, ?config_card),
    EventGroups = [cards:card_event_group(Db, CardKey) || CardKey <- EventGroupCardKeys],
    EventCardKeys = lists:foldl(fun(#card_event_group{events = Keys}, Acc) -> ordsets:union(Keys, Acc) end, ordsets:new(), EventGroups),
    Events = [cards:card_event(Db, CardKey) || CardKey <- EventCardKeys],
    #event_data{super_fields = EventSuperFields, events = Events, groups = EventGroups}.

%% Local functions
