-module(nihao_auth_server).

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
    {nihao_auth_server,
     {nihao_auth_server, start_link, []},
     permanent,
     5000,
     worker,
     [nihao_auth_server]}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({auth, Id, Password}, _From, State) ->
    case catch nihao_user_db:authenticate(Id, Password) of
	{atomic, ok} ->
	    {reply, ok, State};
	{aborted, wrong_password} ->
	    {reply, failed, State};
	{aborted, user_not_found} ->
	    {reply, failed, State};
	{aborted, interr} ->
	    {reply, failed, State}
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

