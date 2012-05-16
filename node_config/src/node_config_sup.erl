
-module(node_config_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    NodeConfig = {node_config, 
		  {node_config, start, []},
		  permanent,
		  5000,
		  worker,
		  [node_config]},
    {ok, { {one_for_one, 5, 10}, [NodeConfig]} }.

