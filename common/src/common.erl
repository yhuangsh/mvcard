-module(common).

-export([now_in_seconds/0]).


now_in_seconds() ->
    Now = calendar:now_to_datetime(now()),
    calendar:datetime_to_gregorian_seconds(Now).
