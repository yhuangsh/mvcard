
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
    Env = application:get_all_env(),
    case proplists:get_value(fenliu, Env, false) of
	{true, Nodes} when is_list(Nodes) ->
	    case lists:member(local, Nodes) of
		true ->
		    Nodes2 = lists:map(fun(local) -> node(); (N) -> N end, Nodes),
		    {ok, _} = fenliu:add_server(baomi_server, Nodes2),
		    {ok, {{one_for_one, 5, 10}, [child_spec()]}};
		false ->
		    {ok, _} = fenliu:add_server(baomi_server, Nodes),
		    {ok, {{one_for_one, 5, 10}, []}}
	    end;
	false ->
	    {ok, {{one_for_one, 5, 10}, [child_spec()]}}
    end.

child_spec() ->
    {baomi_server,
     {baomi_server, start_link, []},
     permanent,
     5000,
     worker,
     [baomi_server]}.

