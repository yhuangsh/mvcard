-module(qb_promo).
-compile(export_all).
-include("qb.hrl").

get(Mid, Uid) -> get_1(qb_tab_promo:read({Mid, Uid})).
get_1({ok, P}) -> P;
get_1({nok, _}) -> undefined.

    
