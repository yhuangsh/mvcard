-module(nihao_session_cache).

-export([init/0,
	 create/3,
	 read/1,
	 update/2,
	 delete/1]).

-define(NIHAO_SESSION_CACHE, nihao_session_cache).

-record(s, {id, expire_time, opaque}).

init() ->
    {atomic, ok} = mnesia:create_table(?NIHAO_SESSION_CACHE, [{attributes, record_info(fields, s)},
							      {record_name, s},
							      {ram_copies, [node()]}]).

create({UserId, ServiceId}, ExpireTime, Opaque) ->
    F = fun() ->
		case mnesia:read(?NIHAO_SESSION_CACHE, {UserId, ServiceId}) of
		    [_Session] ->
			mnesia:abort({nok, session_exist});
		    [] ->
			ok = mnesia:write(?NIHAO_SESSION_CACHE, #s{id={UserId, ServiceId}, expire_time=ExpireTime, opaque=Opaque}),
			{ok, session_created};
		    _Else ->
			mnesia:abort({nok, internal_error})
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.
			    
read({UserId, ServiceId}) ->
    F = fun() ->
		case mnesia:read(?NIHAO_SESSION_CACHE, {UserId, ServiceId}) of
		    [#s{id={UserId, ServiceId}, expire_time=ExpireTime, opaque=Opaque}] ->
			case is_expired(ExpireTime) of
			    true ->
				ok = mnesia:delete(?NIHAO_SESSION_CACHE, {UserId, ServiceId}),
				{nok, {session_expired, Opaque}};
			    false ->
				{ok, {read, Opaque}}
			end;
		    _Else ->
			{nok, session_not_found}
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

update({UserId, ServiceId}, Opaque) ->
    F = fun() ->
		case mnesia:read(?NIHAO_SESSION_CACHE, {UserId, ServiceId}) of
		    [#s{id={UserId, ServiceId}, expire_time=ExpireTime, opaque=_}] ->
			case is_expired(ExpireTime) of
			    true ->
				ok = mnesia:delete(?NIHAO_SESSION_CACHE, {UserId, ServiceId}),
				{nok, {session_expired, Opaque}};
			    false ->
				ok = mnesia:write(?NIHAO_SESSION_CACHE, #s{id={UserId, ServiceId}, expire_time=ExpireTime, opaque=Opaque}),
				{ok, session_updated}
			end;
		    _Else ->
			{nok, session_not_found}
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

delete({UserId, ServiceId}) ->
    F = fun() ->
		ok = mnesia:delete(?NIHAO_SESSION_CACHE, {UserId, ServiceId}),
		{ok, session_deleted}
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_expired(ExpireTime) ->
    common:now_in_seconds() - ExpireTime >= 0.

