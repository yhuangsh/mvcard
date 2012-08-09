-module(qb_trans).
-compile(export_all).

-include("qb.hrl").

new(UserId, MerchantId) -> new_1(qb_tab_trans:create(trans_id(), UserId, MerchantId)).
new_1({ok, T}) -> T;
new_1({nok, _}) -> undefined. 
    
trans_id() -> lists:flatten(io_lib:format("~p-~p-~p", tuple_to_list(now()))).

