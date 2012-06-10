-module(nihao_captcha_pool_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 child_spec/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../common/include/common.hrl").

-define(SERVER, ?MODULE). 

-record(state, {q, capacity}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

child_spec() ->
    {nihao_captcha_pool_server,
     {nihao_captcha_pool_server, start_link, []},
     permanent,
     5000,
     worker,
     [nihao_captcha_pool_server]}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-define(DEFAULT_CAPTCHA_POOL_CAPACITY, (10*?THOUSANDS)).

init([]) ->
    Env = application:get_all_env(),
    Capacity = proplists:get_value(capacity, Env, ?DEFAULT_CAPTCHA_POOL_CAPACITY),
    {ok, #state{q=queue:new(), capacity=Capacity}}.

handle_call(get, _From, State) ->
    {Reply, Q} = queue:out(State#state.q),
    {reply, Reply, State#state{q=Q}}.

handle_cast({add, CaptchaEntries}, State) ->
    case queue:len(State#state.q) + length(CaptchaEntries) < State#state.capacity of
	true ->
	    Q = queue:join(State#state.q, queue:from_list(CaptchaEntries)),
	    {noreply, State#state{q=Q}};
	false ->
	    {noreply, State}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
