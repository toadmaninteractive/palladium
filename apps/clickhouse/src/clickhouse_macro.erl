-module(clickhouse_macro).

%% Include files

%% Exported functions

-export([
    interpolate/1
]).

-define(clickhouse_app, clickhouse).

%% API

-spec interpolate(Sql :: binary()) -> binary().

interpolate(Sql) ->
    replace(Sql, <<>>).

%% Local functions

time_partition_numeric(DateToVar, NumberSeq, day) ->
    Sql = <<"toYYYYMMDD(subtractDays(toDate({to}), {seq}))">>,
    web_util:interpolate(Sql, #{to => DateToVar, seq => NumberSeq});
time_partition_numeric(DateToVar, NumberSeq, week) ->
    Sql = <<"(toYear(subtractWeeks(toDate({to}), {seq})) * 100 + toISOWeek(subtractWeeks(toDate({to}), {seq})))">>,
    web_util:interpolate(Sql, #{to => DateToVar, seq => NumberSeq});
time_partition_numeric(DateToVar, NumberSeq, month) ->
    Sql = <<"toYYYYMM(subtractMonths(toDate({to}), {seq}))">>,
    web_util:interpolate(Sql, #{to => DateToVar, seq => NumberSeq});
time_partition_numeric(DateToVar, NumberSeq, quarter) ->
    Sql = <<"(toYear(subtractQuarters(toDate({to}), {seq})) * 10 + toQuarter(subtractQuarters(toDate({to}), {seq})))">>,
    web_util:interpolate(Sql, #{to => DateToVar, seq => NumberSeq}).

time_partition_fun(DateVar, day) ->
    Sql = <<"toYYYYMMDD({to})">>,
    web_util:interpolate(Sql, #{to => DateVar});
time_partition_fun(DateVar, week) ->
    Sql = <<"(toYear({to}) * 100 + toISOWeek({to}))">>,
    web_util:interpolate(Sql, #{to => DateVar});
time_partition_fun(DateVar, month) ->
    Sql = <<"toYYYYMM({to})">>,
    web_util:interpolate(Sql, #{to => DateVar});
time_partition_fun(DateVar, quarter) ->
    Sql = <<"(toYear({to}) * 10 + toQuarter({to}))">>,
    web_util:interpolate(Sql, #{to => DateVar}).

time_partition_label(DateToVar, NumberSeq, day) ->
    Sql = <<
        "concat(",
            "multiIf(",
                "toMonth(subtractDays(toDate({to}), {seq})) = 1, 'Jan ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 2, 'Feb ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 3, 'Mar ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 4, 'Apr ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 5, 'May ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 6, 'Jun ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 7, 'Jul ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 8, 'Aug ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 9, 'Sep ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 10, 'Oct ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 11, 'Nov ', ",
                "toMonth(subtractDays(toDate({to}), {seq})) = 12, 'Dec ', ",
                "'??? '",
            "), ",
            "toString(toDayOfMonth(subtractDays(toDate({to}), {seq}))), ",
            "', ', ",
            "toString(toYear(subtractDays(toDate({to}), {seq})))",
        ")"
    >>,
    web_util:interpolate(Sql, #{to => DateToVar, seq => NumberSeq});
time_partition_label(DateToVar, NumberSeq, week) ->
    Sql = <<
        "concat(",
            "'Wk ', ",
            "toString(toISOWeek(subtractWeeks(toDate({to}), {seq}))), ",
            "', ', ",
            "toString(toYear(subtractWeeks(toDate({to}), {seq}))) ",
        ")"
    >>,
    web_util:interpolate(Sql, #{to => DateToVar, seq => NumberSeq});
time_partition_label(DateToVar, NumberSeq, month) ->
    Sql = <<
        "concat(",
            "multiIf(",
                "toMonth(subtractMonths(toDate({to}), {seq})) = 1, 'Jan, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 2, 'Feb, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 3, 'Mar, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 4, 'Apr, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 5, 'May, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 6, 'Jun, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 7, 'Jul, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 8, 'Aug, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 9, 'Sep, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 10, 'Oct, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 11, 'Nov, ', "
                "toMonth(subtractMonths(toDate({to}), {seq})) = 12, 'Dec, ', "
                "'???, '"
            "), "
            "toString(toYear(subtractMonths(toDate({to}), {seq})))",
        ")"
    >>,
    web_util:interpolate(Sql, #{to => DateToVar, seq => NumberSeq});
time_partition_label(DateToVar, NumberSeq, quarter) ->
    Sql = <<
        "concat(",
            "toString(toYear(subtractQuarters(toDate({to}), {seq}))), ",
            "' Q', ",
            "toString(toQuarter(subtractQuarters(toDate({to}), {seq})))",
        ")"
    >>,
    web_util:interpolate(Sql, #{to => DateToVar, seq => NumberSeq}).

time_partition_diff(DateFromVar, DateToVar, PartitionBy) ->
    Sql = <<"dateDiff('{partition}', toDate({from}), toDate({to}))">>,
    web_util:interpolate(Sql, #{partition => PartitionBy, from => DateFromVar, to => DateToVar}).

time_partition_join(DatetimeVar, DateToVar, NumberSeq, day) ->
    Sql = <<"toDate({datetime}) = toDate(subtractDays(toDate({to}), {seq}))">>,
    web_util:interpolate(Sql, #{datetime => DatetimeVar, to => DateToVar, seq => NumberSeq});
time_partition_join(DatetimeVar, DateToVar, NumberSeq, week) ->
    Sql = <<"(toYear({datetime}) * 100 + toISOWeek({datetime})) = (toYear(subtractWeeks(toDate({to}), {seq})) * 100 + toISOWeek(subtractWeeks(toDate({to}), {seq})))">>,
    web_util:interpolate(Sql, #{datetime => DatetimeVar, to => DateToVar, seq => NumberSeq});
time_partition_join(DatetimeVar, DateToVar, NumberSeq, month) ->
    Sql = <<"toYYYYMM({datetime}) = toYYYYMM(subtractMonths(toDate({to}), {seq})) ">>,
    web_util:interpolate(Sql, #{datetime => DatetimeVar, to => DateToVar, seq => NumberSeq});
time_partition_join(DatetimeVar, DateToVar, NumberSeq, quarter) ->
    Sql = <<"(toYear({datetime}) * 10 + toQuarter({datetime})) = (toYear(subtractQuarters(toDate({to}), {seq})) * 10 + toQuarter(subtractQuarters(toDate({to}), {seq})))">>,
    web_util:interpolate(Sql, #{datetime => DatetimeVar, to => DateToVar, seq => NumberSeq}).

replace(<<>>, Acc) -> Acc;
replace(<<"$timePartitionNumeric(", Rest/binary>>, Acc) ->
    {Contents, Rest1} = contents(Rest),
    [DateToVar, NumberSeq, PartitionBy|_] = [util_binary:trim(P) || P <- binary:split(Contents, <<",">>, [global])],
    replace(Rest1, <<Acc/binary, (time_partition_numeric(DateToVar, NumberSeq, partition(PartitionBy)))/binary>>);
replace(<<"$timePartitionFun(", Rest/binary>>, Acc) ->
    {Contents, Rest1} = contents(Rest),
    [DateToVar, PartitionBy|_] = [util_binary:trim(P) || P <- binary:split(Contents, <<",">>, [global])],
    replace(Rest1, <<Acc/binary, (time_partition_fun(DateToVar, partition(PartitionBy)))/binary>>);
replace(<<"$timePartitionLabel(", Rest/binary>>, Acc) ->
    {Contents, Rest1} = contents(Rest),
    [DateToVar, NumberSeq, PartitionBy|_] = [util_binary:trim(P) || P <- binary:split(Contents, <<",">>, [global])],
    replace(Rest1, <<Acc/binary, (time_partition_label(DateToVar, NumberSeq, partition(PartitionBy)))/binary>>);
replace(<<"$timePartitionDiff(", Rest/binary>>, Acc) ->
    {Contents, Rest1} = contents(Rest),
    [DateFromVar, DateToVar, PartitionBy|_] = [util_binary:trim(P) || P <- binary:split(Contents, <<",">>, [global])],
    replace(Rest1, <<Acc/binary, (time_partition_diff(DateFromVar, DateToVar, partition(PartitionBy)))/binary>>);
replace(<<"$timePartitionJoin(", Rest/binary>>, Acc) ->
    {Contents, Rest1} = contents(Rest),
    [DatetimeVar, DateToVar, NumberSeq, PartitionBy|_] = [util_binary:trim(P) || P <- binary:split(Contents, <<",">>, [global])],
    replace(Rest1, <<Acc/binary, (time_partition_join(DatetimeVar, DateToVar, NumberSeq, partition(PartitionBy)))/binary>>);
replace(<<Byte:8/integer, Rest/binary>>, Acc) ->
    replace(Rest, <<Acc/binary, Byte:8/integer>>).

contents(Text) -> contents(Text, <<>>).

contents(<<>>, ContentAcc) -> {ContentAcc, <<>>};
contents(<<")", Rest/binary>>, ContentAcc) -> {ContentAcc, Rest};
contents(<<Byte:8/integer, Rest/binary>>, ContentAcc) -> contents(Rest, <<ContentAcc/binary, Byte:8/integer>>).

partition(<<"day">>) -> day;
partition(<<"week">>) -> week;
partition(<<"month">>) -> month;
partition(<<"quarter">>) -> quarter.
