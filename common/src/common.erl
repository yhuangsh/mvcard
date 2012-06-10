-module(common).

-export([now_in_seconds/0,
	date_to_seconds/1,
	datetime_to_seconds/1,
	timestamp_to_seconds/1]).

now_in_seconds() ->
    timestamp_to_seconds(now()).

date_to_seconds({Y, M, D}) ->
    datetime_to_seconds({{Y, M, D}, {0, 0, 0}}).

datetime_to_seconds({{Y, M, D}, {H, Min, S}}) ->
    Dt = {{Y, M, D}, {H, Min, S}},
    calendar:datetime_to_gregorian_seconds(Dt).

timestamp_to_seconds({X, Y, Z}) ->
    Dt = calendar:now_to_datetime({X, Y, Z}),
    calendar:datetime_to_gregorian_seconds(Dt).
