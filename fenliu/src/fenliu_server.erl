%%%-------------------------------------------------------------------
%%% @author  <yonghuan@3CNL09495>
%%% @copyright (C) 2012, 
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2012 by  <yonghuan@3CNL09495>
%%%-------------------------------------------------------------------
-module(fenliu_server).

-behaviour(gen_server).

%% API
-export([start_link/4]).
-export([call/2, call/3, cast/2]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {server_name, node_list, max_fails, call_timeout}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerName, Nodes, MaxFails, CallTimeout) when is_atom(ServerName), is_list(Nodes), is_integer(MaxFails), is_integer(CallTimeout) ->
    gen_server:start_link({local, fenliu:server_name(ServerName)}, ?SERVER, [ServerName, Nodes, MaxFails, CallTimeout], []).

call(ServerName, Request) ->
    gen_server:call(fenliu:server_name(ServerName), {call, Request}).

call(ServerName, Request, Timeout) when is_integer(Timeout) ->
    gen_server:call(fenliu:server_name(ServerName), {call, Request}, Timeout).

cast(ServerName, Msg) ->
    gen_server:cast(fenliu:server_name(ServerName), {cast, Msg}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ServerName, Nodes, MaxFails, CallTimeout]) ->
    NodeList = [{N, MaxFails} || N <- Nodes],
    {ok, #state{server_name = ServerName,
		node_list = NodeList,
		max_fails = MaxFails,
		call_timeout = CallTimeout}}.

handle_call({call, Request}, _From, State) ->
    call_node(Request, State);
handle_call(nodes, _From, State) ->
    NodeList = State#state.node_list,
    {reply, {ok, NodeList}, State}.

handle_cast({cast, Msg}, State) ->
    cast_node(Msg, State);
handle_cast({add_nodes, Nodes}, State) ->
    NodeList = State#state.node_list,
    NewNodeList = [{N, State#state.max_fails} || N <- Nodes],
    {noreply, State#state{node_list = NodeList ++ NewNodeList}}; 
handle_cast({remove_nodes, Nodes}, State) ->
    NodeList = State#state.node_list,
    NewNodeList = lists:filter(fun({N, _}) -> not lists:member(N, Nodes) end, NodeList),
    {noreply, State#state{node_list = NewNodeList}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

call_node(Request, State) ->
    NodeList = State#state.node_list,
    ServerName = State#state.server_name,
    CallTimeout = State#state.call_timeout,
    NodeList = State#state.node_list,
    case call_node_1(ServerName, Request, CallTimeout, NodeList) of
	{ok, Reply, NewNodeList} ->
	    {reply, Reply, State#state{node_list = NewNodeList}};
	{error, run_out_of_nodes} ->
	    {reply, {error, run_out_of_nodes}, State#state{node_list = []}}
    end.

call_node_1(_, _, _, []) -> {error, run_out_of_nodes};
call_node_1(ServerName, Request, CallTimeout, [{_, 0}|T]) -> call_node_1(ServerName, Request, CallTimeout, T); 
call_node_1(ServerName, Request, CallTimeout, [{Node, FailCount}|T]) -> 
     case catch gen_server:call({ServerName, Node}, Request, CallTimeout) of
	{'EXIT', {timeout, Trace}} ->
	    {ok, {'EXIT', {timeout, Trace}}, T ++ [{Node, FailCount - 1}]};
%	{'EXIT', {{nodedown, _}, _}} ->
%	    call_node_1(Request, ServerName, CallTimeout, T ++ [{Node, FailCount - 1}]);
%	{'EXIT', {noproc, _}} ->
%	    call_node_1(Request, ServerName, CallTimeout, T ++ [{Node, FailCount - 1}]);
	{'EXIT', Reason} ->
	    error_logger:warning_msg(io_lib:format("call to {~p, ~p} failed, reaons: ~p~n", [ServerName, Node, Reason])),
	    call_node_1(ServerName, Request, CallTimeout, T ++ [{Node, FailCount - 1}]);
	Reply ->
	    {ok, Reply, T ++ [{Node, FailCount}]}
    end.

cast_node(Msg, State) ->
    NodeList = State#state.node_list,
    ServerName = State#state.server_name,
    {ok, NewNodeList} = cast_node_1(ServerName, Msg, NodeList),
    {noreply, State#state{node_list = NewNodeList}}.

cast_node_1(_, _, []) -> {ok, []};
cast_node_1(ServerName, Msg, [{_, 0}|T]) -> cast_node_1(ServerName, Msg, T);
cast_node_1(ServerName, Msg, [{Node, FailCount}|T]) ->
    gen_server:cast({ServerName, Node}, Msg),
    {ok, T ++ [{Node, FailCount}]}.



