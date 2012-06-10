-module(nihao_captcha_generator).

-export([generate/3,
	 generate_n/3]).

-include("../../common/include/common.hrl").
-define(USE_FENLIU_CAST, true).
-include("../../fenliu/include/fenliu.hrl").

generate(StartTime, CodeLen, Count) ->
    TimeToStart = StartTime - common:now_in_seconds(),
    case TimeToStart > 0 of
	true ->
	    error_logger:info_msg("nihao captcha generator will start in ~p seconds", [TimeToStart]);
	false ->
	    {Time, CaptchaEntries} = timer:tc(generate_n, [CodeLen, Count, []]),
	    error_logger:info_msg("nihao captcha generator generated ~p entries in ~p seconds", [Count, Time/1000]),
	    fl_cast(nihao_captcha_pool_server, {add, CaptchaEntries})
    end.

alphabet() -> "0123456789".
alphabet_len() -> length(alphabet()).

generate_n(_, 0, Acc) -> Acc;
generate_n(L, N, Acc) when L > 0, N > 0 -> 
    NewAcc = [generate_1(L) | Acc],
    generate_n(L, N - 1, NewAcc).

generate_1(L) when L < 10 ->
    Random = binary_to_list(crypto:strong_rand_bytes(L)),

    Alphabet = alphabet(),
    AlphabetLen = alphabet_len(),

    AlphabetPositions = lists:map(fun(X) -> X rem AlphabetLen end, Random),
    Code = lists:map(fun(P) -> lists:nth(P + 1, Alphabet) end, AlphabetPositions),

    Template = "convert -fill '#222222' -size 128x48 -gravity Center -wave 5x100 -swirl 50 -font Courier -pointsize 20 label:~s -draw 'Bezier 10,40 50,35 100,35 150,35 200,50 250,35 300,35' -emboss 10x1 png:-",
    Cmd = io_lib:format(Template, [Code]),
    Image = os:cmd(Cmd),

    Id = crypto:md5([Random, Image, Code]),
    {Id, Code, Image}.

