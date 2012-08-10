-module(qb_session).
-compile(export_all).
-include("qb.hrl").

cookie_name() -> "qb".

get(SessionId) -> get_1(qb_tab_session:read(SessionId)).
get_1({ok, Session}) -> Session;
get_1({nok, _}) -> undefined.

update(Session) -> qb_tab_session:update(Session).

login(UserId, Password) -> login_1(qb_user:get(UserId), Password).
login_1(#qbu{id=UserId, pwd=Password}, Password) -> create(UserId);
login_1(#qbu{id=UserId, pwd=_DifferentPassword}, _Password) -> login_error();
login_1(undefined, _) -> login_error().
login_error() -> {nok, "用户名或密码错"}.

create(UserId) -> create(session_id(UserId), UserId).
create(SessionId, UserId) -> create_1(qb_tab_session:read(SessionId), SessionId, UserId).
create_1({ok, _}, SessionId, _) -> {ok, SessionId};
create_1({nok, _}, SessionId, UserId) -> qb_tab_session:create(SessionId, UserId).

session_id(UserId) -> base64:encode_to_string(UserId).

