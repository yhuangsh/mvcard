-module(stats).

-export([new/0,
	 success_hit/1,
	 failure_hit/1,
	 hit_velocity/0,
	 success_rate/0,
	 failure_rate/0]).

-include("../include/common.hrl").

new() ->
    StartTime = common:now_in_seconds(),
    #server_stats{start_time = StartTime,
		  hits = 0,
		  success = 0}.

success_hit(S) ->
    S#server_stats{hits = S#server_stats.hits + 1,
		   success = S#server_stats.success + 1}.

failure_hit(S) ->
    S#server_stats{hits = S#server_stats.hits + 1}.
    
hit_velocity(S) ->
    Now = now_in_seconds(),
    S#server_state.hits / (Now - S#server_state.start_time).

success_rate(S) ->
    Now = now_in_seconds(),
    S#server_state.success / S#server_state.hits.

failure_rate(S) ->
    1 - success_rate(S).

-define(SUCCESS_HIT(State), State#state{stats = stats:success_hit(State#state.stats)}).
-define(FAILURE_HIT(State), State#state{stats = stats:failure_hit(State#state.stats)}).
