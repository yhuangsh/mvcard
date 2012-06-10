-module(nihao_login_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 child_spec/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

child_spec() ->
    {nihao_login_server,
     {nihao_login_server, start_link, []},
     permanent,
     5000,
     worker,
     [nihao_login_server]}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({login, {UserId, Password}, {ServiceId, ExpireTime, Opaque}}, _From, State) ->
    Reply = case nihao_user_db:verify(UserId, Password) of
		{ok, user_verified} ->
		    nihao_session_cache:create({UserId, ServiceId}, ExpireTime, Opaque);
		Else ->
		    Else
	    end,
    {reply, Reply, State};
handle_call({login, {UserId, Password}, {CaptchaId, CaptchaCode}, {ServiceId, ExpireTime, Opaque}}, _From, State) ->
    Ret1 = nihao_user_db:verify(UserId, Password),
    Ret2 = nihao_captcha_server:verify(CaptchaId, CaptchaCode),
    Reply = case {Ret1, Ret2} of
		{{ok, user_verified}, {ok, captcha_verified}} ->
		    nihao_session_cache:create({UserId, ServiceId}, ExpireTime, Opaque);
		{{ok, user_verified}, CaptchaNOK} ->
		    CaptchaNOK;
		{UserNOK, {ok, captcha_verified}} ->
		    UserNOK;
		_NOK ->
		    {nok, both_failed}
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

