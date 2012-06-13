-module(nihao_session_cache).

-export([init/0,
	 create/3,
	 read/1,
	 update/2,
	 delete/1,
	 sweeper/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(NIHAO_SESSION_CACHE, nihao_session_cache).

-record(s, {id, expire_time, opaque}).

init() ->
    {atomic, ok} = mnesia:create_table(?NIHAO_SESSION_CACHE, [{attributes, record_info(fields, s)},
							      {record_name, s},
							      {ram_copies, [node()]}]).

create({UserId, ServiceId}, ExpireTime, Opaque) ->
    F = fun() ->
		case mnesia:read(?NIHAO_SESSION_CACHE, {UserId, ServiceId}) of
		    [_Session] ->
			mnesia:abort({nok, session_exist});
		    [] ->
			ok = mnesia:write(?NIHAO_SESSION_CACHE, #s{id={UserId, ServiceId}, expire_time=ExpireTime, opaque=Opaque}, write),
			{ok, session_cached};
		    Reason ->
			mnesia:abort({nok, {internal_error, Reason}})
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.
			    
read({UserId, ServiceId}) ->
    F = fun() ->
		case mnesia:read(?NIHAO_SESSION_CACHE, {UserId, ServiceId}) of
		    [#s{id={UserId, ServiceId}, expire_time=ExpireTime, opaque=Opaque}] ->
			case is_expired(ExpireTime) of
			    true ->
				ok = mnesia:delete(?NIHAO_SESSION_CACHE, {UserId, ServiceId}, write),
				{nok, {session_expired, Opaque}};
			    false ->
				{ok, {read, Opaque}}
			end;
		    _Else ->
			{nok, session_not_found}
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

update({UserId, ServiceId}, Opaque) ->
    F = fun() ->
		case mnesia:read(?NIHAO_SESSION_CACHE, {UserId, ServiceId}) of
		    [#s{id={UserId, ServiceId}, expire_time=ExpireTime, opaque=_}] ->
			case is_expired(ExpireTime) of
			    true ->
				ok = mnesia:delete(?NIHAO_SESSION_CACHE, {UserId, ServiceId}, write),
				{nok, {session_expired, Opaque}};
			    false ->
				ok = mnesia:write(?NIHAO_SESSION_CACHE, #s{id={UserId, ServiceId}, expire_time=ExpireTime, opaque=Opaque}, write),
				{ok, session_updated}
			end;
		    _Else ->
			{nok, session_not_found}
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

delete({UserId, ServiceId}) ->
    F = fun() ->
		ok = mnesia:delete(?NIHAO_SESSION_CACHE, {UserId, ServiceId}, write),
		{ok, session_deleted}
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

sweeper(StartTime, Count) ->
    TimeToStart = StartTime - common:now_in_seconds(),
    case TimeToStart > 0 of
	true ->
	    error_logger:info_msg("nihao session cache sweeper will start in ~p seconds", [TimeToStart]),
	    {undefined, Count};
	false ->
	    FirstSession = mnesia:dirty_first(?NIHAO_SESSION_CACHE),
	    error_logger:info_msg("nihao session cache sweeper starts scanning entries for expired sessions and will clean up to up ~p expired sessions", [Count]),
	    {Time, CleanCount} = timer:tc(fun clean_session/3, [FirstSession, Count, 0]),
	    error_logger:info_msg("niihao captcha cache sweeper has cleaned up ~p expired captcha codes in ~p milliseconds", [CleanCount, Time div 1000]),
	    {Time, CleanCount}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_expired(ExpireTime) ->
    common:now_in_seconds() - ExpireTime >= 0.

clean_session('$end_of_table', _, Acc) -> Acc;
clean_session(_, 0, Acc) -> Acc;
clean_session(Session, Count, Acc) when Count > 0 ->
    NextSession = mnesia:dirty_next(?NIHAO_SESSION_CACHE, Session),
    {NewCount, NewAcc} = case mnesia:dirty_read(?NIHAO_SESSION_CACHE, Session) of
				   [#s{id=Session, expire_time=ExpireTime, opaque=_}] ->
				       case common:now_in_seconds() < ExpireTime of
					   true ->
					       {Count, Acc};
					   false ->
					       {ok, session_deleted} = delete(Session),
					       {Count - 1, Acc + 1}
				       end;
				   _Else ->
				       {Count, Acc}
		   end,
    clean_session(NextSession, NewCount, NewAcc).
    
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
      ?T(test4),

%      ?T(test5),
%      ?T(test6),
%      ?T(test7),
%      ?T(test8),

      ?T(cleanup0)
     ]
    }.

setup0() ->
    ok = mnesia:start().
    
cleanup0() ->
    {atomic, ok} = mnesia:delete_table(?NIHAO_SESSION_CACHE),
    ok.

test0() ->
    {atomic, ok} = init(),
    Record = record_info(fields, s),
    Record = mnesia:table_info(?NIHAO_SESSION_CACHE, attributes),
    ok.

test1() ->
    {ok, session_cached} = create({"user1", "service1"}, expire_in(1), "opaque1"),
    {ok, session_cached} = create({"user1", "service2"}, expire_in(1), "opaque2"),
    {ok, session_cached} = create({user2, service3}, expire_in(1), [opaque3]),
    {nok, session_exist} = create({"user1", "service1"}, expire_in(1), "opaque1"),
    {nok, session_exist} = create({"user1", "service2"}, expire_in(1), "opaqueX"),
				  
    ok.

test2() ->
    {ok, {read, "opaque1"}} = read({"user1", "service1"}),
    {ok, {read, "opaque2"}} = read({"user1", "service2"}),
    {ok, {read, [opaque3]}} = read({user2, service3}),
    {ok, session_deleted} = delete({"user1", "service1"}),
    {ok, session_deleted} = delete({"user1", "service2"}),
    {ok, session_deleted} = delete({user2, service3}),
    ok.

test3() ->
    lists:foreach(fun(N) ->
			  Pattern = io_lib:format("opaque_~p", [N]),
			  Session = {io_lib:format("user_~p", [N]),  io_lib:format("service_~p", [N])},
			  {ok, session_cached} = create(Session, expire_in(60), Pattern)
		  end, lists:seq(1, 10)),
    10 = mnesia:table_info(?NIHAO_SESSION_CACHE, size),
    lists:foreach(fun(N) ->
			  Pattern = io_lib:format("opaque_~p", [N]),
			  Session = {io_lib:format("user_~p", [N]),  io_lib:format("service_~p", [N])},
			  {ok, {read, Pattern}} = read(Session),
			  {ok, session_deleted} = delete(Session)
		  end, lists:seq(1, 10)),
    0 =  mnesia:table_info(?NIHAO_SESSION_CACHE, size),
    ok.
			  
test4() ->
    lists:foreach(fun(N) ->
			  Pattern = io_lib:format("opaque_~p", [N]),
			  Session = {io_lib:format("user_~p", [N]),  io_lib:format("service_~p", [N])},
			  {ok, session_cached} = create(Session, expire_in((N rem 2)*60 + 1), Pattern)
		  end, lists:seq(1, 1000)),
    1000 = mnesia:table_info(?NIHAO_SESSION_CACHE, size),
    timer:sleep(1000),
    {_, 400} = sweeper(common:now_in_seconds(), 400),
    {_, 100} = sweeper(common:now_in_seconds(), 200),
    ok.
    
expire_in(M) -> 
    common:now_in_seconds() + M.

-endif.
