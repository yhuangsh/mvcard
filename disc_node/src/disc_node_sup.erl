
-module(disc_node_sup).

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
    DiscNode = {disc_node, 
		{disc_node, start, []},
		permanent,
		2000,
		worker,
		[disc_node]},
    {ok, { {one_for_all, 0, 1}, [DiscNode]} }.

