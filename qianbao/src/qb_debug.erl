-module(qb_debug).
-compile(export_all).

d(Tag, M) -> d(Tag, M, []).
d(Tag, M, Args) when is_list(Args) -> 
    io:format(user, "~n~s: ", [Tag]),
    io:format(user, M, Args).

