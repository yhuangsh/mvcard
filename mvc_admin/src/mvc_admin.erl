-module(mvc_admin).

-export([init/0,
	 create/2,
	 read/1,
	 update/2,
	 delete/1,
	 export/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MERCHANT_INFO_DB, mvc_merchant_info).
-record(m, {id, info}).

-ifdef(TEST).
init() ->	    
    {atomic, ok} = mnesia:create_table(?MERCHANT_INFO_DB, [{attributes, record_info(fields, m)},
							   {record_name, m},
							   {ram_copies, [node()]}]).
-else.
init() ->	    
    {atomic, ok} = mnesia:create_table(?MERCHANT_INFO_DB, [{attributes, record_info(fields, m)},
							   {record_name, m},
							   {disc_copies, [node()]}]).
-endif.

create(Id, Info) ->
    F = fun() ->
		case mnesia:read(?MERCHANT_INFO_DB, Id) of
		    [] ->
			ok = mnesia:write(?MERCHANT_INFO_DB, #m{id=Id, info=Info}, write),
			{ok, created};
		    [_] ->
			{nok, exists};
		    Reason ->
			{nok, {internal_error, Reason}}
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

read(Id) ->
    F = fun() ->
		case mnesia:read(?MERCHANT_INFO_DB, Id) of
		    [] ->
			{nok, not_found};
		    [#m{id=Id, info=Info}] ->
			{ok, {Id, Info}};
		    Reason ->
			{nok, {internal_error, Reason}}
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

update(Id, Info) ->
    F = fun() ->
		ok = mnesia:write(?MERCHANT_INFO_DB, #m{id=Id, info=Info}, write),
		{ok, updated}
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

delete(Id) ->
    F = fun() ->
		case mnesia:read(?MERCHANT_INFO_DB, Id) of
		    [] ->
			{nok, not_found};
		    [_] ->
			ok = mnesia:delete(?MERCHANT_INFO_DB, Id, write),
			{ok, deleted};
		    Reason ->
			{nok, {internal_error, Reason}}
		end
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

export(csv) ->
    F = fun() ->
		First = mnesia:first(?MERCHANT_INFO_DB),
		CSV = export(First, []),
		{ok, CSV}
	end,
    {_, Ret} = mnesia:transaction(F),
    Ret.

export('$end_of_table', Acc) -> Acc;
export(Id, Acc) ->
    [#m{id=Id, info=Info}] = mnesia:read(?MERCHANT_INFO_DB, Id),
    X = if
	    is_list(Info) ->
		Info;
	    is_tuple(Info) ->
		tuple_to_list(Info);
	    true ->
		Info
	end,
    InfoList = [Id|X],
    [$,|Line] = lists:foldl(fun(M, Acc1) -> Acc1 ++ "," ++ M end, [], InfoList),
    export(mnesia:next(?MERCHANT_INFO_DB, Id), Acc ++ [Line]).

%%%===================================================================
%%% Unit Tests
%%%===================================================================

-ifdef(TEST).

-define(T(X), {??X, fun X/0}).

all_test_() ->
    {inorder,
     [
      ?T(setup0),

      ?T(test0),
      ?T(test1),
      ?T(test2),
%      ?T(test3),
%      ?T(test4),

%      ?T(test5),
%      ?T(test6),
%      ?T(test7),
%      ?T(test8),

      ?T(cleanup0)
     ]
    }.

setup0() ->
    ok = mnesia:start(),
    {atomic, ok} = init(),
    ok.
    
cleanup0() ->
    {atomic, ok} = mnesia:delete_table(?MERCHANT_INFO_DB),
    stopped = mnesia:stop(),
    ok.

test0() ->
    {Id, Info} = {"merchant1", {"address1", "phone1", "price1"}},
    {ok, created} = create(Id, Info),
    {ok, {Id, Info}} = read(Id),
    {ok, ["merchant1,address1,phone1,price1"]} = export(csv),
    {ok, deleted} = delete(Id),
    ok.

test1() ->
    {Id1, Info1} = {"merchant1", {"address1", "phone1", "price1"}},
    {Id2, Info2} = {"merchant2", {"address2", "phone2", "price2"}},
    {ok, created} = create(Id1, Info1),
    {ok, created} = create(Id2, Info2),
    {ok, {Id2, Info2}} = read(Id2),
    {ok, {Id1, Info1}} = read(Id1),
    {ok, ["merchant1,address1,phone1,price1",
	  "merchant2,address2,phone2,price2"]} = export(csv),
    ok.

test2() ->
    {Id1, Info3} = {"merchant1", {"address3", "phone3", "price3"}},
    {ok, updated} = update(Id1, Info3),
    {ok, {Id1, Info3}} = read(Id1),
    {ok, ["merchant1,address3,phone3,price3",
	  "merchant2,address2,phone2,price2"]} = export(csv),
    ok.
    
-endif.

