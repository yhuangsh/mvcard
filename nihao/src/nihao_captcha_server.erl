-module(nihao_captcha_server).
-behaviour(gen_server).

%% API
-export([start_link/0,
	 start_link/2,
	 child_spec/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

start_link(Max, TTL) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Max, TTL], []).

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
-define(DEFAULT_TTL, 120).

init([]) ->
    Env = application:get_all_env(),
    Max = proplists:get_value(max_capacity, Env, ?DEFAULT_MAX_CAPACITY),
    TTL = proplists:get_value(ttl, Env, ?DEFAULT_TTL),
    init([Max, TTL]);
init([Max, TTL]) ->
    {ok, #state{max=Max, ttl=TTL}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(get, _From, State) ->
    case nihao_captcha_cache:size() < State#state.max of
	true ->	
	    case fl_call(nihao_captcha_pool_server, get) of
		{ok, {CaptchaId, CaptchaCode, CaptchaImage}} ->
		    {ok, captcha_cached} = nihao_captcha_cache:create(CaptchaId, CaptchaCode, common:now_in_seconds() + State#state.ttl),
		    {reply, {ok, {CaptchaId, CaptchaImage}}, State};
		NOK ->
		    {reply, NOK, State}
	    end;
	false ->
	    {reply, {nok, captcha_cache_full}, State}
    end;
handle_call({verify, {CaptchaId, CaptchaCode}}, _From, State) ->
    {reply, nihao_captcha_cache:verify(CaptchaId, CaptchaCode), State}.
		   
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

reinit_cache() ->
    catch nihao_captcha_cache:delete_cache(),
    {atomic, ok} = nihao_captcha_cache:init().
    
setup0() ->
    ok = mnesia:start(),
    reinit_cache(),
    {ok, _} = nihao_captcha_pool_server:start_link(),
    ok.
    
cleanup0() ->
    stopped = gen_server:call(nihao_captcha_pool_server, stop),
    stopped = gen_server:call(?SERVER, stop),
    {atomic, ok} = nihao_captcha_cache:delete_cache(),
    ok.

test0() ->
    {ok, _} = start_link(100, 60),
    X = {Id, Code, _} = nihao_captcha_generator:generate(6),
    ok = nihao:add_captcha_to_pool([X]),

    {ok, {Id, _}} = nihao:get_captcha(),
    {ok, captcha_verified} = nihao:verify_captcha({Id, Code}),
    {nok, captcha_not_found} = nihao:verify_captcha({Id, Code}),
    {nok, run_out_of_captcha} = nihao:get_captcha(),
    ok.

test1() ->
    X = nihao_captcha_generator:generate(6, 100),
    ok = nihao:add_captcha_to_pool(X),
    IdCodes = [{Id, Code} || {Id, Code, _} <- X],

    lists:foreach(fun(_) ->
			  {ok, _} = nihao:get_captcha()
		  end, lists:seq(1, 100)),
    lists:foreach(fun({Id, Code}) ->
			  {ok, captcha_verified} = nihao:verify_captcha({Id, Code})
		  end, IdCodes),

    {nok, captcha_not_found} = nihao:verify_captcha(hd(IdCodes)),
    {nok, run_out_of_captcha} = nihao:get_captcha(),

    ok.

test2() ->
    stopped = gen_server:call(?SERVER, stop),
    reinit_cache(),
    {ok, _} = start_link(10, 60),
    X = nihao_captcha_generator:generate(6, 20),
    ok = nihao:add_captcha_to_pool(X),
    IdCodes = [{Id, Code} || {Id, Code, _} <- X],
    IdCodes1 = lists:sublist(IdCodes, 10),
    IdCodes2 = lists:sublist(IdCodes, 11, 10),
    
    lists:foreach(fun(_) ->
			  {ok, _} = nihao:get_captcha()
		  end, lists:seq(1, 10)),
    {nok, captcha_cache_full} = nihao:get_captcha(),

    lists:foreach(fun(IdCode) ->
			  {ok, captcha_verified} = nihao:verify_captcha(IdCode)
		  end, IdCodes1),

    lists:foreach(fun(_) ->
			  {ok, _} = nihao:get_captcha()
		  end, lists:seq(1, 10)),
    {nok, captcha_cache_full} = nihao:get_captcha(),
    lists:foreach(fun(IdCode) ->
			  {ok, captcha_verified} = nihao:verify_captcha(IdCode)
		  end, IdCodes2),
    
    ok.

test3() ->
    stopped = gen_server:call(?SERVER, stop),
    reinit_cache(),
    {ok, _} = start_link(10, 1),
    X = nihao_captcha_generator:generate(6, 10),
    ok = nihao:add_captcha_to_pool(X),
    IdCodes = [{Id, Code} || {Id, Code, _} <- X],

    lists:foreach(fun(_) ->
			  {ok, _} = nihao:get_captcha()
		  end, lists:seq(1, 10)),

    timer:sleep(1000),
    
    io:format(user, "start verifying...", []),

    lists:foreach(fun(IdCode) ->
			  {nok, captcha_expired} = nihao:verify_captcha(IdCode)
		  end, IdCodes),

    lists:foreach(fun(IdCode) ->
			  {nok, captcha_not_found} = nihao:verify_captcha(IdCode)
		  end, IdCodes),
    
    ok.
    
-endif.

