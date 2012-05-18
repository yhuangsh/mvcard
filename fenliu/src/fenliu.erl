-module(fenliu).

-export([
	 add_server/1, add_server/2, add_server/3, add_server/4, remove_server/1,
	 get_nodes/1, add_nodes/2, remove_nodes/2,
	 resolve_server/1, server_name/1
	]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
init([]) -> {ok, {}}.
handle_call(stop, _From, State) -> {stop, normal, ok, State};
handle_call({wait, Time}, _From, State) -> timer:sleep(Time), {reply, {ok, Time}, State};
handle_call(Request, _From, State) -> {reply, {ok, Request}, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-endif.


add_server(ServerName) -> add_server(ServerName, [node()]).
add_server(ServerName, Nodes) when is_list(Nodes) -> add_server(ServerName, Nodes, 5, 5000);
add_server(ServerName, MaxFails) when is_integer(MaxFails) -> add_server(ServerName, [node()], MaxFails, 5000).
add_server(ServerName, Nodes, MaxFails) when is_list(Nodes), is_integer(MaxFails) -> add_server(ServerName, Nodes, MaxFails, 5000);
add_server(ServerName, MaxFails, CallTimeout) when is_integer(MaxFails), is_integer(CallTimeout) -> add_server(ServerName, [node()], MaxFails, CallTimeout).
add_server(ServerName, Nodes, MaxFails, CallTimeout) when is_atom(ServerName), is_list(Nodes), is_integer(MaxFails), is_integer(CallTimeout) ->
    ChildSpec = {server_name(ServerName), 
		 {fenliu_server, start_link, [ServerName, Nodes, MaxFails, CallTimeout]},
		 permanent,
		 5000,
		 worker,
		 [fenliu_server]},
    supervisor:start_child(fenliu_sup, ChildSpec).

remove_server(ServerName) ->
    supervisor:terminate_child(fenliu_sup, server_name(ServerName)),
    supervisor:delete_child(fenliu_sup, server_name(ServerName)).

get_nodes(ServerName) ->
    gen_server:call(server_name(ServerName), nodes).

add_nodes(ServerName, Nodes) when is_list(Nodes) ->
    gen_server:cast(server_name(ServerName), {add_nodes, Nodes}).

remove_nodes(ServerName, Nodes) when is_list(Nodes) ->
    gen_server:cast(server_name(ServerName), {remove_nodes, Nodes}).

resolve_server(ServerName) ->
    case whereis(server_name(ServerName)) of
	Pid when is_pid(Pid) ->
	    fenliu_server;
	undefined ->
	    gen_server
    end.

server_name(ServerName) ->
    list_to_atom("fenliu_" ++ atom_to_list(ServerName)).
 
%%%===================================================================
%%% Unit tests
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
      ?T(test4),
      ?T(test5),
      ?T(test6),
      ?T(test7),
%      ?T(test8),

      ?T(cleanup0)
     ]
    }.

setup0() ->
    ok = application:start(fenliu),
    gen_server = resolve_server(simple_server),
    {ok, _} = gen_server:start({local, simple_server}, ?MODULE, [], []),
    {ok, _} = gen_server:start({local, server2}, ?MODULE, [], []),
    {ok, _} = gen_server:start({local, server3}, ?MODULE, [], []),

    ok.

cleanup0() ->
    ok = gen_server:call(simple_server, stop),
    ok = gen_server:call(server2, stop),
    ok = gen_server:call(server3, stop),
    ok = application:stop(fenliu).
    
test0() ->
    {ok, 0} = gen_server:call(simple_server, 0),
    {ok, Pid} = add_server(simple_server),
    Pid = whereis(server_name(simple_server)),
    fenliu_simple_server = server_name(simple_server),
    fenliu_server = resolve_server(simple_server),
    {ok, 1} = fenliu_server:call(simple_server, 1),
    ok.

test1() ->
    ok = remove_server(simple_server),
    {ok, 0} = gen_server:call(simple_server, 0),
    {'EXIT', {noproc, _}} = (catch fenliu_server:call(simple_server, test)),
    ok = fenliu_server:cast(simple_server, test),
    ok.

test2() ->
    Node = node(),

    {ok, _} = add_server(simple_server, 5),
    {ok, 0} = gen_server:call(simple_server, 0),
    {ok, 1} = fenliu_server:call(simple_server, 1),
    {ok, [{Node, 5}]} = get_nodes(simple_server),
    ok.

test3() ->
    Node = node(),

    ok = add_nodes(simple_server, [a@node, b@node, c@node]),
    {ok, [{Node, 5}, {a@node, 5}, {b@node, 5}, {c@node, 5}]} = get_nodes(simple_server),

    ok = remove_nodes(simple_server, [b@node]),
    {ok, [{Node, 5}, {a@node, 5}, {c@node, 5}]} = get_nodes(simple_server),

    ok = remove_nodes(simple_server, [a@node]),
    {ok, [{Node, 5}, {c@node, 5}]} = get_nodes(simple_server),

    ok = remove_nodes(simple_server, [c@node]),
    {ok, [{Node, 5}]} = get_nodes(simple_server),

    ok = remove_nodes(simple_server, [Node]),
    {ok, []} = get_nodes(simple_server),

    {error, run_out_of_nodes} = fenliu_server:call(simple_server, 1),
    ok = fenliu_server:cast(simple_server, 2),
    ok.

test4() ->
    Node = node(),

    ok = add_nodes(simple_server, [Node, bad@node]),
    {ok, [{Node, 5}, {bad@node, 5}]} = get_nodes(simple_server),

    ok = fenliu_server:cast(simple_server, 0),
    {ok, [{bad@node, 5}, {Node, 5}]} = get_nodes(simple_server),

    ok = fenliu_server:cast(simple_server, 0),
    {ok, [{Node, 5}, {bad@node, 5}]} = get_nodes(simple_server),

    ok = fenliu_server:cast(simple_server, 0),
    {ok, [{bad@node, 5}, {Node, 5}]} = get_nodes(simple_server),
    
    ok.

test5() ->
    Node = node(),

    {ok, 1} = fenliu_server:call(simple_server, 1),
    {ok, [{bad@node, 4}, {Node, 5}]} = get_nodes(simple_server),

    {ok, 2} = fenliu_server:call(simple_server, 2),
    {ok, [{bad@node, 3}, {Node, 5}]} = get_nodes(simple_server),

    {ok, 3} = fenliu_server:call(simple_server, 3),
    {ok, [{bad@node, 2}, {Node, 5}]} = get_nodes(simple_server),

    {ok, 4} = fenliu_server:call(simple_server, 4),
    {ok, [{bad@node, 1}, {Node, 5}]} = get_nodes(simple_server),

    {ok, 6} = fenliu_server:call(simple_server, 6),
    {ok, [{bad@node, 0}, {Node, 5}]} = get_nodes(simple_server),

    {ok, 7} = fenliu_server:call(simple_server, 7),
    {ok, [{Node, 5}]} = get_nodes(simple_server),
   
    ok.

test6() ->
    Node = node(),

    {ok, _} = add_server(server2, [bad2@node, Node], 1),
    
    {ok, [{bad2@node, 1}, {Node, 1}]} = get_nodes(server2),

    {ok, 1} = fenliu_server:call(server2, 1),
    {ok, [{bad2@node, 0}, {Node, 1}]} = get_nodes(server2),

    ok = fenliu_server:cast(server2, 3),
    {ok, [{Node, 1}]} = get_nodes(server2),

    ok.

test7() ->    
    {ok, _} = add_server(server3, 1, 10),

    io:format(user, "node=~p~n", [get_nodes(server3)]),
    {ok, 1} = fenliu_server:call(server3, 1),
    {'EXIT', {timeout, _}} = (catch fenliu_server:call(server3, {wait, 100})),
    
    ok.
    
-endif.
