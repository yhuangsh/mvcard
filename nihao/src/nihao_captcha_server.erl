-module(nihao_captcha_server).
-behaviour(gen_server).

%% API
-export([start_link/0,
	 child_spec/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../common/include/common.hrl").
-define(USE_FENLIU_CALL, true).
-include("../../fenliu/include/fenliu.hrl").

-define(SERVER, ?MODULE). 

-record(state, {sweeper, max, ttl}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

child_spec() ->
    {nihao_captcha_server,
     {nihao_captcha_server, start_link, []},
     permanent,
     5000,
     worker,
     [nihao_captcha_server]}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-define(DEFAULT_MAX_CAPACITY, 100000).
-define(DEFAULT_TTL, 2*?MINUTES).

init([]) ->
    Env = application:get_all_env(),
    Max = proplists:get_value(max_capacity, Env, ?DEFAULT_MAX_CAPACITY),
    TTL = proplists:get_value(ttl, Env, ?DEFAULT_TTL),
    {ok, #state{max=Max, ttl=TTL}}.

handle_call(get, _From, State) ->
    {CaptchaId, CaptchaCode, CaptchaImage} = fl_call(nihao_captcha_pool_server, get),
    case nihao_captcha_cache:capacity() < State#state.max of
	true ->
	    ok = nihao_captcha_cache:create(CaptchaId, CaptchaCode, common:now_in_seconds() + State#state.ttl),
	    {reply, {ok, {CaptchaId, CaptchaImage}}, State};
	false ->
	    {reply, {nok, no_capacity}, State}
    end;
handle_call({verify, {CaptchaId, CaptchaCode}}, _From, State) ->
    Reply = case nihao_captcha_cache:read(CaptchaId) of
		{CaptchaId, CaptchaCode, ExpireTime} ->
		    ok = nihao_captcha_cache:delete(CaptchaId),
		    case common:now_in_seconds() - ExpireTime < 0 of
			true ->
			    {ok, captcha_verified};
			false ->
			    {nok, captcha_expired}
		    end;
		_Else ->
		    {nok, captcha_not_verified}
	    end,
    {reply, Reply, State}.
		   
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


