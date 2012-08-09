-module(qb_tab_session).
-compile(export_all).
-include("qb.hrl").

tab() -> qb_tab_session.

init() ->
    ok = mnesia:start(),
    mnesia:delete_table(tab()),
    init_tab().

init_tab() ->
    {atomic, ok} = mnesia:create_table(tab(), [{attributes, record_info(fields, qbs)}, {record_name, qbs}]).

create(SessionId, UserId) ->
    ok = mnesia:dirty_write(tab(), #qbs{id=SessionId, uid=UserId}),
    {ok, SessionId}.

read(SessionId) -> read_1(mnesia:dirty_read(tab(), SessionId)).
read_1([S]) -> {ok, S};
read_1(_) -> {nok, "无此登录记录"}.
	    
update(Session) -> 
    ok = mnesia:dirty_write(tab(), Session),
    {ok, Session}.
    
