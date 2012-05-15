-module(disc_node_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    disc_node_sup:start_link().

stop(_State) ->
    ok.
