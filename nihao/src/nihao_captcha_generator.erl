-module(nihao_captcha_generator).

-export([generate/1,
	 generate/2,
	 generate_and_add/3,
	 generate_n/3,
	 generate_1/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("wx/include/wx.hrl").

-include("../../common/include/common.hrl").
-define(USE_FENLIU_CAST, true).
-include("../../fenliu/include/fenliu.hrl").

generate(CodeLen) -> 
    [X] = generate(CodeLen, 1),
    X.
generate(CodeLen, Count) -> 
    _ = wx:new(),
    Ret = generate_n(CodeLen, Count, []),
    wx:destroy(),
    Ret.

generate_and_add(StartTime, CodeLen, Count) ->
    case common:now_in_seconds() < StartTime of
	true ->
	    ok;
	false ->
	    fl_cast(nihao_captcha_pool_server, {add, generate(CodeLen, Count)})
    end.

alphabet() -> "0123456789".
alphabet_len() -> length(alphabet()).

generate_n(_, 0, Acc) -> Acc;
generate_n(L, N, Acc) when L > 0, N > 0 -> 
    NewAcc = [generate_1(L) | Acc],
    generate_n(L, N - 1, NewAcc).

generate_1(L) when is_integer(L), 0 < L, L =< 10 ->
    Random = binary_to_list(crypto:strong_rand_bytes(L)),

    Alphabet = alphabet(),
    AlphabetLen = alphabet_len(),

    AlphabetPositions = lists:map(fun(X) -> X rem AlphabetLen end, Random),
    Code = lists:map(fun(P) -> lists:nth(P + 1, Alphabet) end, AlphabetPositions),
    
    Image = generate_image(Code),

    Id = crypto:md5([Random, Image, Code]),
    {Id, Code, Image}.

generate_image(Code) ->
    Bm = wxBitmap:new(96, 48),
    Dc = wxMemoryDC:new(),
    ok = wxMemoryDC:selectObject(Dc, Bm),
    wxDC:setBackground(Dc, ?wxWHITE_BRUSH),
    wxDC:clear(Dc),
    wxDC:drawText(Dc, Code, {10, 10}),
    wxMemoryDC:selectObject(Dc, ?wxNullBitmap),
    Im = wxBitmap:convertToImage(Bm),
    wxImage:saveFile(Im, "tmp.png", ?wxBITMAP_TYPE_PNG),
    wxImage:destroy(Im),
    wxBitmap:destroy(Bm),
    wxMemoryDC:destroy(Dc),
    {ok, Bin} = file:read_file("tmp.png"),
    Bin.


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
%      ?T(test2),
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
    ok.
    
cleanup0() ->
    ok.

test0() ->
    lists:foreach(fun(N) -> {_, C, _} = generate(N), N = length(C) end, lists:seq(1, 10)),
    ok.

test1() ->
    X = generate(6, 10),
    10 = length(X),
    [begin 16 = byte_size(I), 6 = length(C) end || {I, C, _} <- X],
    
    ok.

-endif.
