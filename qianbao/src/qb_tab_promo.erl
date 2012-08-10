-module(qb_tab_promo).
-compile(export_all).
-include("qb.hrl").

tab() -> qb_tab_promo.

init() ->
    ok = mnesia:start(),
    mnesia:delete_table(tab()),
    init_tab(),
    init_rows().

init_tab() ->
    {atomic, ok} = mnesia:create_table(tab(), [{attributes, record_info(fields, qbp)}, {record_name, qbp}, {type, bag}]).

init_rows() ->
    ok = mnesia:dirty_write(tab(), #qbp{id={"001", undefined}, pid="001", value=20, desc="点老鸭煲送20元"}),
    ok = mnesia:dirty_write(tab(), #qbp{id={"002", undefined}, pid="001", value=0.05, desc="店庆5周年95折"}),
    ok = mnesia:dirty_write(tab(), #qbp{id={"002", undefined}, pid="002", value=10, desc="下午茶减10元"}),
    ok = mnesia:dirty_write(tab(), #qbp{id={"002", "joe"}, pid="001", value=50, desc="礼券50元"}),
    ok = mnesia:dirty_write(tab(), #qbp{id={"003", undefined}, pid="001", value=5, desc="来就送5元券"}),
    ok = mnesia:dirty_write(tab(), #qbp{id={"003", "joe"}, pid="001", value=0.20, desc="金卡8折"}),
    ok = mnesia:dirty_write(tab(), #qbp{id={"003", "joe"}, pid="002", value=0.10, desc="银卡9折"}).

read({Mid, Uid}) -> read_1(mnesia:dirty_read(tab(), {Mid, Uid})).
read_1([]) -> {nok, "优惠不存在"};
read_1(P) when is_list(P) -> {ok, P}.
