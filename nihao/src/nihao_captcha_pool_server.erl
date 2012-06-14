-module(nihao_captcha_pool_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 child_spec/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(get, _From, State) ->
    case queue:out(State#state.q) of
	{{value, Reply}, Q} -> 
	    {reply, {ok, Reply}, State#state{q=Q}};
	{empty, Q} ->
	    {reply, {nok, run_out_of_captcha}, State#state{q=Q}}
    end.

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
%      ?T(test2),
%      ?T(test3),
%      ?T(test4),

%      ?T(test5),
%      ?T(test6),
%      ?T(test7),
%      ?T(test8),

      ?T(cleanup0)
     ]
    }.

setup0() ->
    {ok, _} = start_link(),
    ok.
    
cleanup0() ->
    stopped = gen_server:call(?SERVER, stop),
    ok.

test0() ->
    X = nihao_captcha_generator:generate(6),
    ok = gen_server:cast(?SERVER, {add, [X]}),
    {ok, X} = gen_server:call(?SERVER, get),
    ok.

test1() ->
    X = nihao_captcha_generator:generate(4, 5),
    ok = gen_server:cast(?SERVER, {add, X}),
    Y = [gen_server:call(?SERVER, get),
	 gen_server:call(?SERVER, get),
	 gen_server:call(?SERVER, get),
	 gen_server:call(?SERVER, get),
	 gen_server:call(?SERVER, get)],
    Y1 = [C || {ok, C} <- Y],
    [] = X -- Y1,
    
    ok.

-endif.
