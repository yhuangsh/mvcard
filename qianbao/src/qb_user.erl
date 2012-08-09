-module(qb_user).
-compile(export_all).
-include("qb.hrl").

get(UserId) -> get_1(qb_tab_user:read(UserId)).
get_1({ok, U}) -> U;
get_1({nok, _}) -> undefined.
    
