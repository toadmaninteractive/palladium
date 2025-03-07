-module(web_rest_callback_dashboard).

%% Include files

-include("card_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    get_dashboard_data/1
]).

%% API

-spec get_dashboard_data(ProjectId :: binary()) ->
    web_protocol:dashboard_data().

get_dashboard_data(ProjectId) ->
    Db = web_analytics:project_db(ProjectId),
    WidgetKeys = card_config:dashboard_widgets(Db),
    Widgets = [cards:card_widget(Db, Key) || Key <- WidgetKeys],
    ParamCardKeys = lists:foldl(fun(#card_widget{params = Keys}, Acc) -> ordsets:union(Keys, Acc) end, ordsets:new(), Widgets),
    Params = [cards:card_query_param(Db, CardKey) || CardKey <- ParamCardKeys],
    #dashboard_data{widgets = Widgets, params = Params}.

%% Local functions
