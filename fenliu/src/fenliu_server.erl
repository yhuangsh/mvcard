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
-export([start_link/2]).
-export([call/2, call/3, cast/2]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {server_name, node_list}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerName, Nodes) when is_atom(ServerName), is_list(Nodes) ->
    gen_server:start_link({local, fenliu:server_name(ServerName)}, ?SERVER, [ServerName, Nodes], []).

call(ServerName, Request) ->
    gen_server:call(fenliu:server_name(ServerName), {call, Request, 5000}).

call(ServerName, Request, Timeout) when is_integer(Timeout) ->
    gen_server:call(fenliu:server_name(ServerName), {call, Request, Timeout}, Timeout + 100).

cast(ServerName, Msg) ->
    gen_server:cast(fenliu:server_name(ServerName), {cast, Msg}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ServerName, Nodes]) ->
    error_logger:info_msg("Fenliu: for ~p at ~p~n", [ServerName, Nodes]),
    {ok, #state{server_name = ServerName,
		node_list = Nodes}}.

handle_call({call, Request, Timeout}, _From, State) ->
    call_node(Request, State, Timeout);
handle_call(nodes, _From, State) ->
    NodeList = State#state.node_list,
    {reply, {ok, NodeList}, State}.

handle_cast({cast, Msg}, State) ->
    cast_node(Msg, State);
handle_cast({add_nodes, Nodes}, State) ->
    NodeList = State#state.node_list,
    {noreply, State#state{node_list = NodeList ++ Nodes}}; 
handle_cast({remove_nodes, Nodes}, State) ->
    NodeList = State#state.node_list,
    NewNodeList = lists:filter(fun(N) -> not lists:member(N, Nodes) end, NodeList),
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

call_node(Request, State, Timeout) ->
    NodeList = State#state.node_list,
    ServerName = State#state.server_name,
    case call_node_1(ServerName, Request, Timeout, NodeList) of
	{ok, Reply, NewNodeList} ->
	    {reply, Reply, State#state{node_list = NewNodeList}};
	{error, run_out_of_nodes} ->
	    {reply, {error, run_out_of_nodes}, State#state{node_list = []}}
    end.

call_node_1(_, _, _, []) -> {error, run_out_of_nodes};
call_node_1(ServerName, Request, Timeout, [Node|T]) -> 
     case catch gen_server:call({ServerName, Node}, Request, Timeout) of
	 {'EXIT', {timeout, _}} ->
	     error_logger:warning_msg(io_lib:format("call to {~p, ~p} time out~n", [ServerName, Node])),
	     call_node_1(ServerName, Request, Timeout, T);
	 {'EXIT', {noproc, _}} ->
	     error_logger:warning_msg(io_lib:format("server ~p does not exist~n", [ServerName])),
	     call_node_1(ServerName, Request, Timeout, T);
	 {'EXIT', {{nodedown, _}, _}} ->
	     error_logger:warning_msg(io_lib:format("node ~p is down~n", [Node])),
	     call_node_1(ServerName, Request, Timeout, T);
	 {'EXIT', Reason} ->
	     error_logger:warning_msg(io_lib:format("call to {~p, ~p} failed, reaons: ~p~n", [ServerName, Node, Reason])),
	     call_node_1(ServerName, Request, Timeout, T);
	Reply ->
	    {ok, Reply, T ++ [Node]}
    end.

cast_node(Msg, State) ->
    NodeList = State#state.node_list,
    ServerName = State#state.server_name,
    {ok, NewNodeList} = cast_node_1(ServerName, Msg, NodeList),
    {noreply, State#state{node_list = NewNodeList}}.

cast_node_1(_, _, []) -> {ok, []};
cast_node_1(ServerName, Msg, [Node|T]) ->
    gen_server:cast({ServerName, Node}, Msg),
    {ok, T ++ [Node]}.



