-module(nihao_user_db).

-export([init/0, init/1,
	 authenticate/2,
	 create/4, 
	 read/1, 
	 update/4,
	 delete/2]).

-define(NIHAO_USER_TABLE, nihao_user_table).

-record(user, {id,
	       email,
	       mobile,
	       salt,
	       pwd_hash,
	       services}).

init() -> init(disc_copies).
init(CopyType) ->
    {atomic, ok} = mnesia:create_table(?NIHAO_USER_TABLE, [{attributes, record_info(fields, user)},
							   {CopyType, [node()]},
							   {record_name, user}]).

authenticate(Id, Password) ->
    F = fun() ->
		case mnesia:read(?NIHAO_USER_TABLE, Id) of
		    [User] ->
			case is_password_match(User, Password) of
			    true ->
				ok;
			    _ ->
				mnesia:abort(wrong_password)
			end;
		    [] ->
			mnesia:abort(user_not_found);
		    Reason ->
			error_logger:error_msg("Internal error ~p~n", [Reason]),
			mnesia:abort(interr)
		end
	end,
    mnesia:transaction(F).

create(Id, Email, Mobile, Password) ->
    F = fun() ->
		case mnesia:read(?NIHAO_USER_TABLE, Id) of
		    [] ->
			{Salt, PwdHash} = new_pwd_hash(Password),
			UserRec = #user{id = Id, 
					email = Email, 
					mobile = Mobile, 
					salt = Salt, 
					pwd_hash = PwdHash},
			ok = mnesia:write(?NIHAO_USER_TABLE, UserRec);
		    [_User] ->
			mnesia:abort(user_exits);
		    Reason ->
			error_logger:error_msg("Internal error ~p~n", [Reason]),
			mnesia:abort(interr)
		end
	end,
    mnesia:transaction(F).

read(Id) ->
    F = fun() ->
		case mnesia:read(?NIHAO_USER_TABLE, Id) of
		    [] ->
			mnesia:abort(user_not_found);
		    [User] ->
			to_tuple(User);
		    Reason ->
			error_logger:error_msg("Internal error ~p~n", [Reason]),
			mnesia:abort(interr)
		end
	end,
    mnesia:transaction(F).

update(Id, Email, Mobile, Password) ->		   
    F = fun() ->
		case mnesia:read(?NIHAO_USER_TABLE, Id) of
		    [User] ->
			case is_password_match(User, Password) of
			    true ->
				NewRec = update_rec(User, Email, Mobile, Password),
				mnesia:write(?NIHAO_USER_TABLE, NewRec);
			    false ->
				mnesia:abort(wrong_password)
			end;
		    [] ->
			mnesia:abort(user_not_found);
		    Reason ->
			error_logger:error_msg("Internal error ~p~n", [Reason]),
			mnesia:abort(interr)
		end
	end,
    mnesia:transaction(F).
	
delete(Id, Password) ->
    F = fun() ->
		case mnesia:read(?NIHAO_USER_TABLE, Id) of
		    [User] ->
			case is_password_match(User, Password) of
			    true ->
				mnesia:delete(?NIHAO_USER_TABLE, Id);
			    false ->
				mnesia:abort(wrong_password)
			end;
		    [] ->
			mnesia:abort(user_not_found);
		    Reason ->
			error_logger:error_msg("Internal error ~p~n", [Reason]),
			mnesia:abort(interr)
		end
	end,
    mnesia:transaction(F).

%% Internal Funs

new_pwd_hash(Password) ->
    Salt = crypto:strong_rand_bytes(16),
    PwdHash = gen_pwd_hash(Password, Salt),
    {Salt, PwdHash}.

gen_pwd_hash(P, S) -> crypto:sha([S, <<"salted password">>, P]).

to_tuple(User) -> {User#user.id, User#user.email, User#user.mobile}.

is_password_match(User, Password) ->
    Salt = User#user.salt,
    PwdHash = gen_pwd_hash(Password, Salt),
    PwdHash =:= User#user.pwd_hash.

update_rec(U0, Email, Mobile, Password) -> 
    U1 = do_email(U0, Email),
    U2 = do_mobile(U1, Mobile),
    U3 = do_password(U2, Password),
    U3.

do_email(U, nochange) -> U;
do_email(U, Email) -> U#user{email = Email}.

do_mobile(U, nochange) -> U;
do_mobile(U, Mobile) -> U#user{mobile = Mobile}.

do_password(U, nochange) -> U;
do_password(U, Password) ->
    {Salt, PwdHash} = new_pwd_hash(Password),
    U#user{salt = Salt, pwd_hash = PwdHash}.
    
