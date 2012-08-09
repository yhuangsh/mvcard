-module(qb_merchant).
-compile(export_all).
-include("qb.hrl").

get(MerchantId) -> get_1(qb_tab_merchant:read(MerchantId)).
get_1({ok, M}) -> M;
get_1(_) -> undefined.
