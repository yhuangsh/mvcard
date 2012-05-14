
-module(fenliu_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([add_server/1, add_server/2, add_server/3, add_server/4, remove_server/1]).
%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_server(ServerName) -> add_server(ServerName, [node()]).
add_server(ServerName, Nodes) -> add_server(ServerName, Nodes, 5, 5000).
add_server(ServerName, Nodes, MaxFails) -> add_server(ServerName, Nodes, MaxFails, 5000).
add_server(ServerName, Nodes, MaxFails, CallTimeout) when is_atom(ServerName), is_list(Nodes), is_integer(MaxFails), is_integer(CallTimeout) ->
    supervisor:start_child(?MODULE, [ServerName, Nodes, MaxFails, CallTimeout]).

remove_server(ServerName) ->
    supervisor:terminate_child(?MODULE, whereis(fenliu:server_name(ServerName))).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [child_spec()]} }.

child_spec() ->
    {fenliu_server,
     {fenliu_server, start_link, []},
     permanent,
     5000,
     worker,
     [fenliu_server]}.

