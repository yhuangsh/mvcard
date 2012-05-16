-module(node_config).

-export([start/0, run/1]).

start() ->
    {ok, spawn(?MODULE, run, [application:get_all_env()])}.

run(Envs) when is_list(Envs) ->
    SchemaLoc = proplists:get_value(mnesia_schema_location, Envs, current),

    change_mnesia_schema_copy_type(SchemaLoc),
    ok.

change_mnesia_schema_copy_type(current) -> ok;
change_mnesia_schema_copy_type(ram) ->
    ThisNode = node(),
    RamCopies = mnesia:table_info(schema, ram_copies),
    true == lists:member(ThisNode, RamCopies) 
	orelse begin 
		   error_logger:error_msg("Mnesia schema not in ram"),
		   init:stop()
	       end,
    ok;
change_mnesia_schema_copy_type(disc) ->
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

    
			  
