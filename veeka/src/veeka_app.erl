-module(veeka_app).

-behaviour(application).

-include("veeka.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    veeka_sup:start_link().

stop(_State) ->
    ok.

