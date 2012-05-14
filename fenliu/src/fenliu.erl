-module(fenliu).

-export([resolve_server/1, server_name/1]).

resolve_server(ServerName) ->
    case whereis(server_name(ServerName)) of
	Pid when is_pid(Pid) ->
	    fenliu_server;
	undefined ->
	    gen_server
    end.

server_name(ServerName) ->
    list_to_atom("fenliu_" ++ atom_to_list(ServerName)).
 
