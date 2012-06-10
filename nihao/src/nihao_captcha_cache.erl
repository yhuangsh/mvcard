-module(nihao_captcha_cache).

-export([init/0,
	 create/3,
	 read/1,
	 delete/1,
	 capacity/0,
	 sweeper/1]).

-define(NIHAO_CAPTCHA_CACHE, nihao_captcha_cache).

-record(c, {id, code, expire_time}).

init() ->
    {atomic, ok} = mnesia:create_table(?NIHAO_CAPTCHA_CACHE, [{attributes, record_info(fields, c)}, 
								    {record_name, c},
								    {ram_copies, [node()]}]).

create(CaptchaId, CaptchaCode, ExpireTime) -> 
    mnesia:dirty_write(?NIHAO_CAPTCHA_CACHE, #c{id=CaptchaId, code=CaptchaCode, expire_time=ExpireTime}).

read(CaptchaId) ->
    case mnesia:dirty_read(?NIHAO_CAPTCHA_CACHE, CaptchaId) of
	[#c{id=CaptchaId, code=CaptchaCode, expire_time=ExpireTime}] ->
	    {CaptchaId, CaptchaCode, ExpireTime};
	Else ->
	    Else
    end.

delete(CaptchaId) ->
    mnesia:dirty_delete(?NIHAO_CAPTCHA_CACHE, CaptchaId).

capacity() ->
    mnesia:table_info(?NIHAO_CAPTCHA_CACHE, size).

%%%===================================================================
%%% Internal functions
%%%===================================================================

sweeper(StartTime) ->
    TimeToStart = StartTime - common:now_in_seconds(),
    case TimeToStart > 0 of
	true ->
	    error_logger:info_msg("nihao captcha cache sweeper will start in ~p seconds", [TimeToStart]);
	false ->
	    CaptchaIds = mnesia:dirty_all_keys(?NIHAO_CAPTCHA_CACHE),
	    error_logger:info_msg("nihao_captcha cache sweeper starts scanning ~p entries for expired captcha codes", [length(CaptchaIds)]),
	    SweepFn = fun(CaptchaId, Acc) ->
			      case mnesia:dirty_read(CaptchaId) of
				  [#c{id=CaptchaId, code=_, expire_time=ExpireTime}] ->
				      case common:now_in_seconds()- ExpireTime < 0 of
					  true ->
					      Acc;
					  false ->
					      mnesia:dirty_delete(?NIHAO_CAPTCHA_CACHE, CaptchaId),
					      Acc + 1
				      end;
				  _Else ->
				      Acc
			      end 
		      end,
	    {Time, Count} = timer:tc(lists, foldl, [SweepFn, 0, CaptchaIds]),
	    error_logger:info_msg("niihao captcha cache sweeper has cleaned up ~p expired captcha codes in ~p microseconds", [Count, Time])
    end.
