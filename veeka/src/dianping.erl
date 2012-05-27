-module(dianping).
-export([search_init/0, search_store/2]).
-compile(export_all).

-define(NEXT_PAGE, [19979,19968,39029]).
-define(SOUSUO, [25628,32034]).

search_init() ->
    contact_server().

search_store(City, StoreName) ->
    Url = make_search_url(City, StoreName),
    %io:format("url=~ts~n", [Url]),
    store_list(Url, []).

store_list(Url, Acc) ->
    Req = {Url, headers()},
    case httpc:request(get, Req, [], []) of
	{ok, {{_, 200, "OK"}, _, Body}} ->
	    case catch parse_store_info(Body) of
		{next_page, _, NewUrl, _, {_, _, StoreList}} ->
		    store_list(NewUrl, Acc ++ StoreList);
   		{ok, {_, _, StoreList}, _} ->
		    Acc ++ StoreList;
		Else ->
		    error_logger:error_msg("page parsing error: ~p", [Else]),
		    Acc
	    end;
	Else ->
	    error_logger:error_msg("request failed: ~p~n", [Else]),
	    Acc
    end.

parse_store_info(Body) when is_list(Body) ->
    Bin = list_to_binary(Body),
    Opts = [{event_fun, fun handler/3},
	    {event_state, {expect_store, undefined, []}},
	    skip_external_dtd],
    xmerl_sax_parser:stream(Bin, Opts).

handler({startElement, _, "a", _, [{[], [], "href", "RsDetail.aspx?id=" ++ Rest}]}, _, {expect_store, undefined, Acc}) ->
    %io:format("StoreId: ~p ~n", [StoreId]),
    {StoreId, _} = string:to_integer(Rest),
    {expect_store_name, StoreId, Acc};
handler({characters, InputStoreName}, _, {expect_store_name, StoreId, Acc}) ->
    StoreName = trim_newline(InputStoreName),
    %io:format("StoreName: ~ts ~n", [unicode:characters_to_binary(StoreName)]),
    {expect_store_price, {StoreId, StoreName}, Acc};
handler({characters, InputStorePrice}, _, {expect_store_price, {StoreId, StoreName}, Acc}) ->
    StorePrice = trim_newline(InputStorePrice),
    %io:format("StorePrice: ~ts ~n", [unicode:characters_to_binary(StorePrice)]),
    {expect_store_address, {StoreId, StoreName, StorePrice}, Acc};
handler({characters, InputStoreAddress}, _, {expect_store_address, {StoreId, StoreName, StorePrice}, Acc}) ->
    StoreAddress = trim_newline(InputStoreAddress),
    %io:format("StoreAddress: ~ts ~n", [unicode:characters_to_binary(StoreAddress)]),
    {expect_store, undefined, Acc ++ [{StoreId, StoreName, StorePrice, StoreAddress}]};
handler({startElement, _, "a", _, [{[], [], "href", "/wap2/sekey.aspx?" ++ RestUrl}, {[], [], "title", ?NEXT_PAGE}]}, _, {expect_store, _, _}) -> 
    %io:format("Next Page encountered ~p, !~n", [RestUrl]),
    throw({next_page, "http://wap.dianping.com/wap2/sekey.aspx?"++RestUrl});
handler(_, _, State) ->
    State.

trim_newline(L) when is_list(L) ->
    L1 = trim_leading_newline(L),
    L2 = trim_leading_newline(lists:reverse(L1)),
    lists:reverse(L2).

trim_leading_newline(L) ->
    lists:dropwhile(fun(10) -> true; (_) -> false end, L).
			    
contact_server() ->
    httpc:set_options([{cookies, enabled}]),
    Req0 = {"http://wap.dianping.com", headers()},
    {ok, {{_, 200, "OK"}, _, _}} = httpc:request(get, Req0, [], []),  
    ok.

headers() ->	 
    [
     {"User-Agent", "NokiaN73-2/2.0626 S60/3.0 Profile/MIDP-2.0 Configuration/CLDC-1.1"}
    ].
			     
make_search_url(City, Name) when is_list(Name) ->
    "http://wap.dianping.com/wap2/sekey.aspx?cd="++city_code(City)++"&key="++escape_uri(Name)++"&btnSearch="++escape_uri(?SOUSUO).

city_code(shanghai) -> "1";
city_code(beijing) -> "2".


escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(<<C, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $A].
