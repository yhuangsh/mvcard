-module(qb_tab_user).
-compile(export_all).
-include("qb.hrl").

tab() -> qb_tab_user.

init() ->
    ok = mnesia:start(),
    mnesia:delete_table(tab()),
    init_tab(),
    init_rows().

init_tab() ->
    {atomic, ok} = mnesia:create_table(tab(), [{attributes, record_info(fields, qbu)}, {record_name, qbu}, {disc_copies, [node()]}]).

init_rows() ->
    ok = mnesia:dirty_write(tab(), #qbu{id="joe", pwd="12345", pts=2000}),
    ok = mnesia:dirty_write(tab(), #qbu{id="david", pwd="12345", pts=500}),
    ok = mnesia:dirty_write(tab(), #qbu{id="erik", pwd="12345", pts=340}).

read(UserId) -> read_1(mnesia:dirty_read(tab(), UserId)).
read_1([U]) -> {ok, U};
read_1(_) -> {nok, "用户名或密码错"}.
			
