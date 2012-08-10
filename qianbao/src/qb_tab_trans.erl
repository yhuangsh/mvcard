-module(qb_tab_trans).
-compile(export_all).
-include("qb.hrl").

%% transaction state: 
%% initiated -> billed    -> paid           -> confirmed  -> disputed
%%           -> cancelled -> cancelled      -> disputed   
%%           -> expired   -> expired

tab() -> qb_tab_trans.

init() ->
    ok = mnesia:start(),
    mnesia:delete_table(tab()),
    init_tab().

init_tab() ->
    mnesia:create_table(tab(), [{attributes, record_info(fields, qbt)}, {record_name, qbt}, {disc_copies, [node()]}]).

create(TransId, UserId, MerchantId) ->
    T = #qbt{id=TransId, state=initiated, uid=UserId, mid=MerchantId},
    ok = mnesia:dirty_write(tab(), T),
    {ok, T}.

read(TransId) -> read_1(mnesia:dirty_read(tab(), TransId)).
read_1([T]) -> {ok, T};
read_1(_) -> {nok, "没有这笔交易"}.



