-module(nihao_captcha_cache).

-export([init/0,
	 create/3,
	 verify/2,
	 size/0,
	 sweeper/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(NIHAO_CAPTCHA_CACHE, nihao_captcha_cache).

-record(c, {id, code, expire_time}).

init() ->
    {atomic, ok} = mnesia:create_table(?NIHAO_CAPTCHA_CACHE, [{attributes, record_info(fields, c)}, 
							      {record_name, c},
							      {ram_copies, [node()]}]).

create(CaptchaId, CaptchaCode, ExpireTime) -> 
    ok = mnesia:dirty_write(?NIHAO_CAPTCHA_CACHE, #c{id=CaptchaId, code=CaptchaCode, expire_time=ExpireTime}),
    {ok, captcha_cached}.

verify(CaptchaId, CaptchaCode) ->
    case mnesia:dirty_read(?NIHAO_CAPTCHA_CACHE, CaptchaId) of
	[] ->
	    {nok, captcha_not_found};
	[#c{id=CaptchaId, code=CaptchaCode, expire_time=ExpireTime}] ->
	    ok = mnesia:dirty_delete(?NIHAO_CAPTCHA_CACHE, CaptchaId),		    
	    case common:now_in_seconds() - ExpireTime < 0 of
		true ->
		    {ok, captcha_verified};
		false ->
		    {nok, captcha_expired}
	    end;
	[_OtherCaptcha] ->
	    {nok, captcha_not_verified};
	NOK ->
	    {nok, {internal_error, NOK}}
    end.

size() ->
    mnesia:table_info(?NIHAO_CAPTCHA_CACHE, size).

sweeper(StartTime) ->
    TimeToStart = StartTime - common:now_in_seconds(),
    case TimeToStart > 0 of
	true ->
	    error_logger:info_msg("nihao captcha cache sweeper will start in ~p seconds", [TimeToStart]),
	    {undefined, 0};
	false ->
	    CaptchaIds = mnesia:dirty_all_keys(?NIHAO_CAPTCHA_CACHE),
	    error_logger:info_msg("nihao_captcha cache sweeper starts scanning ~p entries for expired captcha codes", [length(CaptchaIds)]),
	    SweepFn = fun(CaptchaId, Acc) ->
			      case mnesia:dirty_read(?NIHAO_CAPTCHA_CACHE, CaptchaId) of
				  [#c{id=CaptchaId, code=_, expire_time=ExpireTime}] ->
				      case common:now_in_seconds()- ExpireTime < 0 of
					  true ->
					      Acc;
					  false ->
					      ok = mnesia:dirty_delete(?NIHAO_CAPTCHA_CACHE, CaptchaId),
					      Acc + 1
				      end;
				  _Else ->
				      Acc
			      end 
		      end,
	    {Time, Count} = timer:tc(lists, foldl, [SweepFn, 0, CaptchaIds]),
	    error_logger:info_msg("niihao captcha cache sweeper has cleaned up ~p expired captcha codes in ~p milliseconds", [Count, Time div 1000]),
	    {Time, Count}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
    {atomic, ok} = mnesia:delete_table(?NIHAO_CAPTCHA_CACHE),
    ok.

test0() ->
    {atomic, ok} = init(),
    Record = record_info(fields, c),
    Record = mnesia:table_info(?NIHAO_CAPTCHA_CACHE, attributes),
    ok.

test1() ->
    {ok, captcha_cached} = create("id1", "code1", expire_in(1)),
    timer:sleep(1500),
    {_, 1} = sweeper(common:now_in_seconds()),
    ok.

test2() ->
    {nok, captcha_not_found} = verify("id1", "code1"),
    {ok, captcha_cached} = create("id2", "code2", expire_in(1)),
    timer:sleep(1100),
    {nok, captcha_expired} = verify("id2", "code2"),
    ok.

test3() ->
    lists:foreach(fun(N) ->
			  {ok, captcha_cached} = create(io_lib:format("id~p", [N]), 
							io_lib:format("code~p", [N]),
							expire_in(60))
		  end, lists:seq(1, 10)),
    10 = size(),
    lists:foreach(fun(N) ->
			  {ok, captcha_verified} = verify(io_lib:format("id~p", [N]),
							  io_lib:format("code~p", [N])),
			  Count = 10 - N,
			  Count = size()
		  end, lists:seq(1, 10)),
    0 = size(),
    ok.
			  
test4() ->
    lists:foreach(fun(N) ->
			  {ok, captcha_cached} = create(io_lib:format("id~p", [N]), 
							io_lib:format("code~p", [N]),
							expire_in((N rem 2)*60 + 1))
		  end, lists:seq(1, 1000)),
    1000 = size(),
    timer:sleep(1000),
    {_, 500} = sweeper(common:now_in_seconds()),
    ok.
    
expire_in(M) -> 
    common:now_in_seconds() + M.

-endif.
