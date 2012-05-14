
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
    StartType = application:get_env(start_type),
    KeyFile = application:get_env(key_file),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [StartType, KeyFile]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([StartType, KeyFile]) ->
    {ok, { {one_for_one, 5, 10}, [child_spec(StartType, KeyFile)]} }.

child_spec(StartType, KeyFile) ->
    {baomi_server,
     {baomi_server, start_link, [StartType, KeyFile]},
     permanent,
     5000,
     worker,
     [baomi_server]}.

