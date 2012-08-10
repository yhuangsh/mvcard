-module(qb_tab_merchant).
-compile(export_all).
-include("qb.hrl").

tab() -> qb_tab_merchant.

init() ->
    ok = mnesia:start(),
    mnesia:delete_table(tab()),
    init_tab(),
    init_rows().

init_tab() ->
    {atomic, ok} = mnesia:create_table(tab(), [{attributes, record_info(fields, qbm)}, {record_name, qbm}, {disc_copies, [node()]}]).

init_rows() ->
    ok = mnesia:dirty_write(tab(), #qbm{id="001", name="小南国", addr="浦东新区陆家嘴西路168号正大广场9楼", rebate=0.5}),
    ok = mnesia:dirty_write(tab(), #qbm{id="002", name="俏江南", addr="静安区延安中路881号", rebate=0.3}),
    ok = mnesia:dirty_write(tab(), #qbm{id="003", name="大董烤鸭", addr="东城区金宝街88号金宝汇购物中心5楼", rebate=0.4}).

read(MerchantId) -> read_1(mnesia:dirty_read(tab(), MerchantId)).
read_1([M]) -> {ok, M};
read_1(_) -> {nok, "商户不存在"}.

