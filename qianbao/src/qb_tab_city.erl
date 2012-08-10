-module(qb_tab_city).
-compile(export_all).
-include("qb.hrl").

tab() -> qb_tab_city.

init() ->
    ok = mnesia:start(),
    mnesia:delete_table(tab()),
    init_tab(),
    init_rows().

init_tab() ->
    {atomic, ok} = mnesia:create_table(tab(), [{attributes, record_info(fields, qbc)}, {record_name, qbc}, {disc_copies, [node()]}]).

init_rows() ->
    ok = mnesia:dirty_write(tab(), #qbc{city="北京", dummy=0}),
    ok = mnesia:dirty_write(tab(), #qbc{city="上海", dummy=0}),
    ok = mnesia:dirty_write(tab(), #qbc{city="广州", dummy=0}),
    ok = mnesia:dirty_write(tab(), #qbc{city="深圳", dummy=0}),
    ok = mnesia:dirty_write(tab(), #qbc{city="重庆", dummy=0}).

read(City) -> read_1(mnesia:dirty_read(tab(), City)).
read_1([C]) -> {ok, C#qbc.city};
read_1(_) -> {nok, "没有这个城市"}.
