-module(disc_node).

-export([start/0, run/0]).

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    ThisNode = node(),
    RamCopies = mnesia:table_info(schema, ram_copies),
    DiscCopies = mnesia:table_info(schema, disc_copies),
    DiscOnlyCopies = mnesia:table_info(schema, disc_only_copies),
    case lists:member(ThisNode, RamCopies) of
	true ->
	    {atomic, ok} = mnesia:change_table_copy_type(schema, ThisNode, disc_copies),
	    error_logger:info_msg("Mnesia schema moved to disc");
	false ->
	    case lists:member(ThisNode, DiscCopies) of
		true ->
		    error_logger:info_msg("Mnesia schema already on disc");
		false ->
		    case lists:member(ThisNode, DiscOnlyCopies) of
			true ->
			    error_logger:info_msg("Mnesia schema alreadu on disc only");
			false ->
			    error_logger:error_msg("No mnesia schema found")
		    end
	    end
    end.
			
    
