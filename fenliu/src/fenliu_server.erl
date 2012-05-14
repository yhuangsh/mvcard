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
-export([call/2, call/3, cast/2, nodes/1, add_nodes/2, delete_nodes/2]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {server_name, node_list, max_fails, call_timeout}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerName, Nodes, MaxFails, CallTimeout) when is_atom(ServerName), is_list(Nodes) ->
    gen_server:start_link({local, fenliu:server_name(ServerName)}, [ServerName, Nodes, MaxFails, CallTimeout], []).

call(ServerName, Request) ->
    gen_server:call(fenliu:server_name(ServerName), {call, Request}).

call(ServerName, Request, Timeout) when is_integer(Timeout) ->
    gen_server:call(fenliu:server_name(ServerName), {call, Request}, Timeout).

cast(ServerName, Msg) ->
    gen_server:cast(fenliu:server_name(ServerName), {cast, Msg}).

nodes(ServerName) ->
    gen_server:call(fenliu:server_name(ServerName), nodes).

add_nodes(ServerName, Nodes) when is_list(Nodes) ->
    gen_server:cast(fenliu:server_name(ServerName), {add_nodes, Nodes}).

delete_nodes(ServerName, Nodes) when is_list(Nodes) ->
    gen_server:cast(fenliu:server_name(ServerName), {delete_nodes, Nodes}).

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
    NodeList = State#state.node_list,
    ServerName = State#state.server_name,
    case pop_a_node(NodeList) of
	{Node, NodeList2} ->
	    gen_server:cast({ServerName, Node}, Msg),
	    NodeList3 = NodeList2 ++ [Node],
	    {noreply, State#state{node_list = NodeList3}};
	empty ->
	    {norpely, State#state{node_list = []}}
    end;
handle_cast({add_nodes, Nodes}, State) ->
    NodeList = State#state.node_list,
    NewNodeList = [{N, State#state.max_fails} || N <- Nodes],
    {noreply, State#state{node_list = NodeList ++ NewNodeList}}; 
handle_cast({delete_nodes, Nodes}, State) ->
    NodeList = State#state.node_list,
    NewNodeList = lists:dropwhile(fun({N, _}) -> lists:member(N, Nodes) end, NodeList),
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
    case pop_a_node(NodeList) of
	{Node, NodeList2} ->
	    case catch gen_server:call({ServerName, Node}, Request, CallTimeout) of
		{'EXIT', {timeout, _}} ->
		    NodeList3 = NodeList2 ++ dec_fail_count(Node),
		    call_node(Request, State#state{node_list = NodeList3});
		Reply ->
		    NodeList3 = NodeList2 ++ [Node],
		    {reply, Reply, State#state{node_list = NodeList3}}
	    end;
	empty ->
	    {reply, {error, no_node}, State#state{node_list = []}}
    end.

pop_a_node([]) -> empty;
pop_a_node([{_, 0}|T]) -> pop_a_node(T);
pop_a_node([H|T]) -> {H, T}.

dec_fail_count({_, 0}) -> [];
dec_fail_count({N, FailCount}) -> [{N, FailCount - 1}].

