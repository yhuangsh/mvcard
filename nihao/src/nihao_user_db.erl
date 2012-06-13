-module(nihao_user_db).

-export([init/0, init/1, init/2,
	 create/2,
	 update/2,
	 verify/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
			ok = mnesia:write(?NIHAO_USER_DB, #u{id=Id, salt=Salt, pwd_hash=PwdHash}, write),
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
			ok = mnesia:write(?NIHAO_USER_DB, #u{id=Id, salt=Salt, pwd_hash=PwdHash}, write),
			{ok, user_password_updated};
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

%%%===================================================================
%%% Unit Tests
%%%===================================================================
 
-ifdef(TEST).

-define(T(X), {??X, fun X/0}).

all_test_() ->
    {inorder,
     [
      ?T(setup0),

      ?T(test0),
      ?T(test1),
      ?T(test2),
      ?T(test3),
%      ?T(test4),

%      ?T(test5),
%      ?T(test6),
%      ?T(test7),
%      ?T(test8),

      ?T(cleanup0)
     ]
    }.

setup0() ->
    ok = mnesia:start(),
    case mnesia:table_info(schema, disc_copies) of
	[] ->
	    {atomic, ok} = mnesia:change_table_copy_type(schema, node(), disc_copies);
	_Else ->
	    ok
    end,
    S = sets:from_list(mnesia:system_info(tables)),
    case sets:is_element(?NIHAO_USER_DB, S) of
	true ->
	    {atomic, ok} = mnesia:delete_table(?NIHAO_USER_DB);
	false ->
	    ok
    end,
    ok.

cleanup0() ->
    {atomic, ok} = mnesia:delete_table(?NIHAO_USER_DB),
    ok.

test0() ->
    {atomic, ok} = init(),
    Record = record_info(fields, u),
    Record = mnesia:table_info(?NIHAO_USER_DB, attributes).

test1() ->
    {ok, user_created} = create("user1", "user1_password"),
    {ok, user_verified} = verify("user1", "user1_password"),
    {nok, user_exist} = create("user1", "user1_password"),
    {nok, user_exist} = create("user1", "wrong_password"),
    {nok, user_wrong_password} = verify("user1", "wrong_password"),

    {nok, user_not_found} = update("no_such_user", "any_password"),
    {nok, user_not_found} = verify("no_such_user", "any_password"),

    {ok, user_password_updated} = update("user1", "user1_new_password"),
    {ok, user_verified} = verify("user1", "user1_new_password"),
    {nok, user_wrong_password} = verify("user1", "user1_password"),
    {ok, user_password_updated} = update("user1", "user1_password"),

    ok.

test2() ->
    {ok, user_created} = create("baduser", "baduser_password"),
    {ok, user_verified} = verify("baduser", "baduser_password"),
    [R] = mnesia:dirty_read(?NIHAO_USER_DB, "baduser"),
    ok = mnesia:dirty_write(?NIHAO_USER_DB, R#u{salt = <<"bad_salt">>}),
    {nok, user_wrong_password} = verify("baduser", "baduser_password"),
    ok = mnesia:dirty_delete(?NIHAO_USER_DB, "baduser"),

    ok.

test3()->
    {ok, user_verified} = verify("user1", "user1_password"),
    [#u{id=_, salt=Salt1, pwd_hash=PwdHash1}] = mnesia:dirty_read(?NIHAO_USER_DB, "user1"),
    {ok, user_password_updated} = update("user1", "user1_password"),
    {ok, user_verified} = verify("user1", "user1_password"),
    [#u{id=_, salt=Salt2, pwd_hash=PwdHash2}] = mnesia:dirty_read(?NIHAO_USER_DB, "user1"),
    true = (Salt1 =/= Salt2) andalso (PwdHash1 =/= PwdHash2),
    
    {ok, user_password_updated} = update("user1", "newpassword"),
    {ok, user_verified} = verify("user1", "newpassword"),
    [#u{id=_, salt=Salt3, pwd_hash=PwdHash3}] = mnesia:dirty_read(?NIHAO_USER_DB, "user1"),
    true = (Salt1 =/= Salt3) andalso (PwdHash1 =/= PwdHash3) andalso (Salt2 =/= Salt3) andalso (PwdHash2 =/= PwdHash3),
    
    {ok, user_created} = create("user2", "user2_password"),
    {ok, user_verified} = verify("user2", "user2_password"),
    [#u{id=_, salt=Salt4, pwd_hash=PwdHash4}] = mnesia:dirty_read(?NIHAO_USER_DB, "user2"),
    true = (Salt1 =/= Salt4) andalso (PwdHash1 =/= PwdHash4)
	andalso (Salt2 =/= Salt4) andalso (PwdHash2 =/= PwdHash4)
	andalso (Salt3 =/= Salt4) andalso (PwdHash3 =/= PwdHash4),

    ok.

-endif.
