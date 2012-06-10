-module(nihao_user_db).

-export([init/0, init/1,
	 create/2,
	 update/2,
	 verify/2,
	 delete/2]).

-define(NIHAO_USER_DB, nihao_user_db).

-record(u, {id, salt, pwd_hash}).

init() ->
    {atomic, ok} = mnesia:create_table(?NIHAO_USER_DB, [{attributes, record_info(fields, u)},
							{disc_copies, [node()]},
							{record_name, u}]).

init(Node) -> init(Node, ram_copies).
init(Node, ram_copies) -> init_1(Node, ram_copies);
init(Node, disc_copies) -> init_1(Node, disc_copies).
init_1(Node, CopyType) ->
    {atomic, ok} = mnesia:add_table_copy(?NIHAO_USER_DB, Node, CopyType).


create(Id, Password) ->
    F = fun() ->
		case mnesia:read(?NIHAO_USER_DB, Id) of
		    [] ->
			{Salt, PwdHash} = new_pwd_hash(Password),
			ok = mnesia:write(?NIHAO_USER_DB, #u{id=Id, salt=Salt, pwd_hash=PwdHash}),
			{ok, user_created};
		    [_User] ->
			mnesia:abort({nok, user_exist});
		    NOK ->
			mnesia:abort({nok, {internal_error, NOK}})
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

update(Id, NewPassword) ->
    F = fun() ->
		case mnesia:read(?NIHAO_USER_DB, Id) of
		    [] ->
			{nok, user_not_found};
		    [_User] ->
			{Salt, PwdHash} = new_pwd_hash(NewPassword),
			ok = mnesia:write(?NIHAO_USER_DB, #u{id=Id, salt=Salt, pwd_hash=PwdHash}),
			{ok, user_password_updated};
		    NOK ->
			mnesia:abort({nok, {internal_error, NOK}})
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

delete(Id, Password) ->
    F = fun() ->
		case mnesia:read(?NIHAO_USER_DB, Id) of
		    [User] ->
			case is_password_match(User, Password) of
			    true ->
				ok = mnesia:delete(?NIHAO_USER_DB, Id),
				{ok, user_deleted};
			    false ->
				mnesia:abort({nok, user_wrong_password})
			end;
		    [] ->
			mnesia:abort({nok, user_not_found});
		    NOK ->
			mnesia:abort({nok, {internal_error, NOK}})
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

verify(Id, Password) ->
    F = fun() ->
		case mnesia:read(?NIHAO_USER_DB, Id) of
		    [User] ->
			case is_password_match(User, Password) of
			    true ->
				{ok, user_verified};
			    _ ->
				mnesia:abort({nok, wrong_password})
			end;
		    [] ->
			mnesia:abort({nok, user_not_found});
		    NOK ->
			mnesia:abort({nok, {internal_error, NOK}})
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_pwd_hash(Password) ->
    Salt = crypto:strong_rand_bytes(16),
    PwdHash = gen_pwd_hash(Password, Salt),
    {Salt, PwdHash}.

gen_pwd_hash(P, S) -> 
    crypto:sha([S, <<"salted password">>, P]).

is_password_match(#u{id=_, salt=Salt, pwd_hash=StoredPwdHash}, Password) ->
    StoredPwdHash =:= gen_pwd_hash(Password, Salt).

