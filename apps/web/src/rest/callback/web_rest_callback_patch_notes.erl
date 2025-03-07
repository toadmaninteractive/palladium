-module(web_rest_callback_patch_notes).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("session.hrl").
-include("settings.hrl").
-include("card_protocol.hrl").
-include("data_protocol.hrl").

%% Exported functions

-export([
    get_patch_notes/1
]).

%% API

-spec get_patch_notes(ProjectId :: binary()) ->
    data_protocol:collection(card_protocol:card_patch_note()).

get_patch_notes(ProjectId) ->
    Db = web_analytics:project_db(ProjectId),
    #card_config{patch_notes = PatchNoteCardKeys} = cards:card_config(Db, ?config_card),
    PatchNotes = [cards:card_patch_note(Db, PNCK) || PNCK <- PatchNoteCardKeys],
    #collection{items = PatchNotes}.

%% Local functions
