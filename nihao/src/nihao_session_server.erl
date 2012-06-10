-module(nihao_session_server).

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
    {nihao_session_server,
     {nihao_session_server, start_link, []},
     permanent,
     5000,
     worker,
     [nihao_session_server]}.
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({change_password, {UserId, ServiceId}, {CaptchaId, CaptchaCode}, {OldPassword, NewPassword}}, _From, State) ->
    Reply = case nihao_session_cache:read({UserId, ServiceId}) of
		{ok, read, _} ->
		    case nihao:verify_captcha({CaptchaId, CaptchaCode}) of
			{ok, captcha_verified} ->
			    case nihao_user_db:verify(UserId, OldPassword) of
				{ok, user_verified} ->
				    nihao_user_db:update(UserId, NewPassword);
				NOK ->
				    NOK
			    end;
			NOK ->
			    NOK
		    end;
		NOK ->
		    NOK
	    end,
    {reply, Reply, State};
handle_call({get_opaque, {UserId, ServiceId}}, _From, State) ->
    {reply, nihao_session_cache:read({UserId, ServiceId}), State};
handle_call({set_opaque, {UserId, ServiceId}, Opaque}, _From, State) ->
    Reply = case nihao_session_cache:read({UserId, ServiceId}) of
		{ok, {read, Opaque}} ->
		    {ok, session_updated};
		{ok, {read, _OldOpaque}} ->
		    nihao_session_cache:update({UserId, ServiceId}, Opaque);
		NOK ->
		    NOK
	    end,
    {reply, Reply, State}.

handle_cast({logout, {UserId, ServiceId}}, State) ->
    nihao_session_cache:delete({UserId, ServiceId}),
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
