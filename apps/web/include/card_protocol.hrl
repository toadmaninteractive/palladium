%% Author: Igor compiler
%% Compiler version: igorc 2.1.4
%% DO NOT EDIT THIS FILE - it is machine generated

-record(card_config, {
    id :: atom(),
    project_name :: binary(),
    databases :: [binary()],
    clickhouse_server = default :: card_protocol:clickhouse_server(),
    dashboard_widgets = [] :: [card_protocol:widget_key()],
    metrics = [] :: [card_protocol:metric_key()],
    maps = [] :: [card_protocol:map_key()],
    parameters = [] :: [card_protocol:query_param_key()],
    queries = [] :: [card_protocol:query_key()],
    event_super_fields :: [binary()],
    event_groups = [] :: [card_protocol:event_group_key()],
    patch_notes = [] :: [card_protocol:patch_note_key()]
}).

-record(card_metric, {
    id :: atom(),
    name :: binary()
}).

-record(card_map, {
    id :: atom(),
    name :: binary(),
    key :: binary(),
    url :: binary(),
    min_x :: float(),
    min_y :: float(),
    max_x :: float(),
    max_y :: float()
}).

-record(card_query_param, {
    id :: atom(),
    name :: binary(),
    key :: binary(),
    type :: data_protocol:query_param_type(),
    input_sql :: binary() | 'undefined',
    value_type :: data_protocol:primitive_type() | 'undefined',
    values :: [data_protocol:string_point()] | 'undefined',
    depends_on :: [card_protocol:query_param_key()] | 'undefined'
}).

-record(card_query, {
    id :: atom(),
    name :: binary(),
    key :: binary(),
    description :: binary(),
    metrics = [] :: [card_protocol:metric_key()],
    params = [] :: [card_protocol:query_param_key()],
    required_params = [] :: [card_protocol:query_param_key()],
    nodes = [] :: [data_protocol:query_node()]
}).

-record(card_event, {
    id :: atom(),
    name :: binary(),
    description :: binary(),
    added_at :: iso8601:datetimems(),
    is_implemented :: boolean(),
    is_tested :: boolean(),
    is_activated :: boolean() | 'undefined',
    fields = [] :: [binary()]
}).

-record(card_event_group, {
    id :: atom(),
    name :: binary(),
    description :: binary(),
    events = [] :: [card_protocol:event_key()]
}).

-record(card_patch_note, {
    id :: atom(),
    title :: binary(),
    description :: binary(),
    added_at :: iso8601:datetimems(),
    analytics_version :: igor_types:int() | 'undefined',
    build_revision :: igor_types:int() | 'undefined'
}).

-record(card_widget, {
    id :: atom(),
    key :: binary(),
    width_cols = 3 :: igor_types:int(),
    params = [] :: [card_protocol:query_param_key()],
    query :: data_protocol:query_node()
}).

