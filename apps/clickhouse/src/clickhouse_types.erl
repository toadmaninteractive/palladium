-module(clickhouse_types).

%% Include files

-include("clickhouse_protocol.hrl").

%% Exported functions

-export([
    type_spec_from_json/1,
    type_spec_to_json/1
]).

%% API

-spec type_spec_from_json(RawTypeSpec :: binary()) ->
    clickhouse_protocol:type_spec().

type_spec_from_json(RawTypeSpec) ->
    parse_type_spec(RawTypeSpec, #type_spec{raw_spec = RawTypeSpec}).

-spec type_spec_to_json(TypeSpec :: clickhouse_protocol:type_spec()) ->
    binary().

type_spec_to_json(#type_spec{raw_spec = RawSpec}) ->
    RawSpec.

%% Local functions

parse_type_spec(<<"Nullable(", Rest/binary>>, TypeSpec) ->
    parse_type_spec(trim_last(Rest, 1), TypeSpec#type_spec{nullable = true});
parse_type_spec(<<"Int8">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = int8};
parse_type_spec(<<"Int16">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = int16};
parse_type_spec(<<"Int32">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = int32};
parse_type_spec(<<"Int64">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = int64};
parse_type_spec(<<"UInt8">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = uint8};
parse_type_spec(<<"UInt16">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = uint16};
parse_type_spec(<<"UInt32">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = uint32};
parse_type_spec(<<"UInt64">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = uint64};
parse_type_spec(<<"Float32">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = float32};
parse_type_spec(<<"Float64">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = float64};
parse_type_spec(<<"Decimal(", Rest/binary>>, TypeSpec) ->
    [Precision, Scale|_] = binary:split(trim_last(Rest, 1), <<",">>, [global]),
    TypeSpec#type_spec{data_type = decimal, decimal_precision = to_integer(Precision), decimal_scale = to_integer(Scale)};
parse_type_spec(<<"Decimal32(", Rest/binary>>, TypeSpec) ->
    TypeSpec#type_spec{data_type = decimal32, decimal_scale = to_integer(trim_last(Rest, 1))};
parse_type_spec(<<"Decimal64(", Rest/binary>>, TypeSpec) ->
    TypeSpec#type_spec{data_type = decimal64, decimal_scale = to_integer(trim_last(Rest, 1))};
parse_type_spec(<<"Decimal128(", Rest/binary>>, TypeSpec) ->
    TypeSpec#type_spec{data_type = decimal128, decimal_scale = to_integer(trim_last(Rest, 1))};
parse_type_spec(<<"String">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = string};
parse_type_spec(<<"FixedString(", Rest/binary>>, TypeSpec) ->
    TypeSpec#type_spec{data_type = fixed_string, fixed_string_length = to_integer(trim_last(Rest, 1))};
parse_type_spec(<<"UUID">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = uuid};
parse_type_spec(<<"Date">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = date};
parse_type_spec(<<"DateTime">>, TypeSpec) ->
    TypeSpec#type_spec{data_type = date_time};
parse_type_spec(<<"Enum8(", Rest/binary>>, TypeSpec) ->
    Items = [binary:split(util_binary:trim(Item), <<"=">>, [global]) || Item <- binary:split(trim_last(Rest, 1), <<",">>, [global])],
    Pairs = [#enum_pair{key = trim_both(util_binary:trim(K), 1), value = to_integer(V)} || [K, V|_] <- Items],
    TypeSpec#type_spec{data_type = enum8, enum_items = Pairs};
parse_type_spec(<<"Enum16(", Rest/binary>>, TypeSpec) ->
    Items = [binary:split(util_binary:trim(Item), <<"=">>, [global]) || Item <- binary:split(trim_last(Rest, 1), <<",">>, [global])],
    Pairs = [#enum_pair{key = trim_both(util_binary:trim(K), 1), value = to_integer(V)} || [K, V|_] <- Items],
    TypeSpec#type_spec{data_type = enum16, enum_items = Pairs};
parse_type_spec(<<"Array(", Rest/binary>>, TypeSpec) ->
    ArrayItemTypeSpec = trim_last(Rest, 1),
    TypeSpec#type_spec{data_type = array, array_item_type = parse_type_spec(ArrayItemTypeSpec, #type_spec{raw_spec = ArrayItemTypeSpec})};
parse_type_spec(<<"AggregateFunction(", _/binary>>, TypeSpec) ->
    TypeSpec#type_spec{data_type = aggregate_function};
parse_type_spec(<<"Tuple(", Rest/binary>>, TypeSpec) ->
    TupleTypeSpec = [util_binary:trim(Item) || Item <- binary:split(trim_last(Rest, 1), <<",">>, [global])],
    TypeSpec#type_spec{data_type = tuple, tuple_item_types = [parse_type_spec(Item, #type_spec{raw_spec = Item}) || Item <- TupleTypeSpec]};
parse_type_spec(_, TypeSpec) ->
    TypeSpec.

trim_last(Binary, N) ->
    binary:part(Binary, 0, byte_size(Binary) - N).

trim_both(Binary, N) ->
    binary:part(Binary, N, byte_size(Binary) - N * 2).

to_integer(Binary) ->
    binary_to_integer(util_binary:trim(Binary)).
