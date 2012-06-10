-module(nihao_scheduled_activity_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 child_spec/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {captcha_cache_sweeper, captcha_generator}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

child_spec() ->
    {nihao_scheduled_activity_server,
     {nihao_scheduled_activity_server, start_link, []},
     permanent,
     5000,
     worker,
     [nihao_scheduled_activity_server]}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({start_captcha_cache_sweeper, TimeToStart, Interval}, _From, State) ->
    case State#state.captcha_cache_sweeper of
	undefined ->
	    case timer:apply_interval(Interval, nihao_captcha_cache, sweeper, [common:now_in_seconds() + TimeToStart]) of
		{ok, Tref} ->
		    {reply, {ok, started}, State#state{captcha_cache_sweeper=Tref}};
		{error, Reason} ->
		    {reply, {nok, Reason}, State}
	    end;
	_Tref ->
	    {reply, {nok, already_started}, State}
    end;
handle_call(stop_captcha_cache_sweeper, _From, State) ->
    case State#state.captcha_cache_sweeper of
	undefined ->
	    {reply, ok, State};
	Tref ->
	    case timer:cancel(Tref) of
		{ok, cancel} ->
		    {reply, {ok, stopped}, State#state{captcha_cache_sweeper=undefined}};
		{error, Reason} ->
		    {reply, {nok, Reason}, State}
	    end
    end;
handle_call({start_captcha_generator, TimeToStart, Interval, CodeLen, Count}, _From, State) ->
    case State#state.captcha_generator of
	undefined ->
	    case timer:apply_interval(Interval, nihao_captcha_generator, generate, [common:now_in_seconds() + TimeToStart, CodeLen, Count]) of
		{ok, Tref} ->
		    {reply, {ok, started}, State#state{captcha_generator=Tref}};
		{error, Reason} ->
		    {reply, {nok, Reason}, State}
	    end;
	_Tref ->
	    {reply, {nok, already_started}, State}
    end;
handle_call(stop_captcha_generator, _From, State) ->
    case State#state.captcha_generator of
	undefined ->
	    {reply, ok, State};
	Tref ->
	    case timer:cancel(Tref) of
		{ok, cancel} ->
		    {reply, {ok, stopped}, State#state{captcha_generator=undefined}};
		{error, Reason} ->
		    {reply, {nok, Reason}, State}
	    end
    end.

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
