
-module(baomi_sup).

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
    {ok, { {one_for_one, 5, 10}, [child_spec()]} }.

child_spec() ->
    {baomi_server,
     {baomi_server, start_link, []},
     permanent,
     5000,
     worker,
     [baomi_server]}.

