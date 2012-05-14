-module(baomi_server).
-author(yong.david.huang@nokia.com).

-behaviour(gen_server).

-include("baomi_def.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/2, start_link/3, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(KEYPOOL_TABLE, nihao_key_pool).

-define(DEFAULT_CAPACITY, 1*?MILLION).
-define(DEFAULT_TIMEOUT, (0*?DAYS + 12*?HOURS + 0*?MINUTES + 0*?SECONDS)).

-define(MINKEYSIZE, (64 div 8)).
-define(MAXKEYSIZE, (256 div 8)).
-define(KEYHEADER, "KXCHv1..").
-define(KEYRANDOMSIZE, 8).

-record(key, {id, 
	      value, 
	      expire_time}).

-record(state, {public_key, 
		private_key, 
		capacity, 
		timeout,
		nodes}).


%%%===================================================================
%%% API
%%%===================================================================

start_link(StartType, KeyFile) ->
    start_link(StartType, KeyFile, []).

start_link(StartType, KeyFile, Options) ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [StartType, KeyFile, Options], []).

stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([StartType, KeyFile, Options]) ->
    Capacity = proplists:get_value(capacity, Options, ?DEFAULT_CAPACITY),
    Timeout = proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT),

    PrivateKey = get_private_key_from_file(KeyFile),
    PublicKey = #'RSAPublicKey'{modulus = PrivateKey#'RSAPrivateKey'.modulus, 
				publicExponent = PrivateKey#'RSAPrivateKey'.publicExponent},
    
    nihao_util:ensure_mnesia_is_running(),
    case StartType of
	recover ->
	    nihao_util:ensure_mnesia_table_exists(?KEYPOOL_TABLE),
	    nihao_util:ensure_mnesia_table_structure(?KEYPOOL_TABLE, key);
	lead ->
	    nihao_util:ensure_mnesia_no_table_exists(?KEYPOOL_TABLE),
	    nihao_util:ensure_mnesia_create_table_ok(?KEYPOOL_TABLE, [ {attributes, record_info(fields, key)},
								       {ram_copies, [node()]},
								       {record_name, key} ]);
	join ->
	    nihao_util:ensure_mnesia_no_table_exists(?KEYPOOL_TABLE),
	    nihao_util:ensure_mnesia_replication_ok(?KEYPOOL_TABLE, ram_copies);
	_Else ->
	    throw({error, "wrong start type"})
    end,
    {ok, #state{public_key = PublicKey, private_key = PrivateKey, capacity = Capacity, timeout = Timeout}}.
	
%% retrieved public key shall be used as in the following Java code
%% 
%% byte[] keyMaterial = /* bytes retrieved from server */
%% KeyFactory keyFactory = KeyFactory.getInstance("RSA"); 
%% X509EncodedKeySpec keySpec = new X509EncodedKeySpec(keyMaterial);
%% PublicKey pk = keyFactory.generatePublic(keySpec);
%% Cipher cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding");
%% cipher.init(Cipher.ENCRYPT_MODE, pk);
%% byte[] PlainText = /* plain text byte array */;
%% byte[] Cryptogram = cipher.doFinal(PlainText);
handle_call(get_public_key, _From, State) ->
    {'SubjectPublicKeyInfo', Der, _} = public_key:pem_entry_encode('SubjectPublicKeyInfo', state_public_key(State)),
    {reply, {ok, Der}, State};

handle_call({exchange_key, EncryptedKeyBlob}, _From, State) ->
    try
	check_capacity(State),
	M = public_key:decrypt_private(EncryptedKeyBlob, state_private_key(State)),
	{Random, KeyValue} = validate_key_blob(M),
	Key = new_key(KeyValue, state_timeout(State)),
	F = fun() -> mnesia:write(?KEYPOOL_TABLE, Key, write) end,
	{atomic, ok} = mnesia:transaction(F),
	{key_id(Key), Random}
    of
	{KeyId, R} ->
	    {reply, {ok, KeyId, R}, State}
    catch
	_Class:_Reason ->
	    %io:format(user, "~w ~w ~n", [Reason, erlang:get_stacktrace()]),
	    error_logger:warning_msg("exchange key failed"),
	    {reply, failed, State}
    end;

handle_call({get_key, KeyId}, _From, State) ->
    try
	Key = read_key(KeyId),
	delete_key_if_expired(Key),
	key_value(Key)
    of
	KeyValue ->
	    {reply, {ok, KeyValue}, State}
    catch
	_Class:_Reason ->
	    error_logger:error_msg("get key failed"),
	    {reply, failed, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(purge_expired_keys, State) ->
    try
	PurgeOneKey = 
	    fun(Key, _Acc) ->
		    case nihao_util:now_in_seconds() >= key_expire_time(Key) of
			true ->
			    mnesia:delete(?KEYPOOL_TABLE, key_id(Key), write),
			    _Acc;
			false ->
			    _Acc
		    end
	    end,
	PurgeKeys = fun() -> mnesia:foldl(PurgeOneKey, 0, ?KEYPOOL_TABLE, write) end,
	mnesia:transaction(PurgeKeys)
    of
	_Any ->
	    {noreply, State}
    catch
	_Class:_Reason ->
	    error_logger:error_msg("problem in purging expired keys"),
	    {noreply, State}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

state_public_key(State) -> State#state.public_key.
state_private_key(State) -> State#state.private_key.
state_capacity(State) -> State#state.capacity.
state_timeout(State) -> State#state.timeout.

key_id(Key) -> Key#key.id.
key_value(Key) -> Key#key.value.
key_expire_time(Key) -> Key#key.expire_time.

get_private_key_from_file(KeyFile) ->
    {ok, PemBin} = file:read_file(KeyFile),
    [{_, PrivateKeyDER, _}] = public_key:pem_decode(PemBin),
    public_key:der_decode('RSAPrivateKey', PrivateKeyDER).

check_capacity(State) ->
    true = (mnesia:table_info(?KEYPOOL_TABLE, size) < state_capacity(State)).

validate_key_blob(KeyBlob) ->
    <<Head:8/bytes, KeySize, Rest/binary>> = KeyBlob,
    true = (<<?KEYHEADER>> =:= Head),
    true = (KeySize =< ?MAXKEYSIZE andalso KeySize >= ?MINKEYSIZE),
    <<Random:?KEYRANDOMSIZE/bytes, Rest2/binary>> = Rest,
    true = (KeySize =< length(binary_to_list(Rest2))),
    <<KeyValue:KeySize/bytes, _Rest2/binary>> = Rest2,
    {Random, KeyValue}.
	    
new_key(KeyValue, Timeout) ->
    Id = crypto:rand_bytes(16),
    ExpireTime = nihao_util:now_in_seconds() + Timeout,
    #key{id = Id, value = KeyValue, expire_time = ExpireTime}.

read_key(KeyId) ->
    F = fun() -> mnesia:read(?KEYPOOL_TABLE, KeyId, read) end,
    {atomic, [Key]} = mnesia:transaction(F),
    Key.

delete_key_if_expired(Key) ->
    case nihao_util:now_in_seconds() >= key_expire_time(Key) of
	true ->
	    F = fun() -> mnesia:delete(?KEYPOOL_TABLE, key_id(Key), write) end,
	    ok = mnesia:transaction(F);
	false ->
	    ok
    end.
    
%%%===================================================================
%%% Unit tests
%%%===================================================================

-ifdef(TEST).

-define(KEYFILE, "C:/Users/yonghuan/huang/erlang/nihao/testkey0").
-define(T(X), {??X, fun X/0}).

all_test_() ->
    {inorder,
     [
      ?T(setup0),

      ?T(test0),
      ?T(test1),
      ?T(test2),
      
      ?T(test3),
      ?T(test4),

      ?T(test5),
      ?T(test6),
      ?T(test7),
      ?T(test8),

      ?T(cleanup0)
     ]
    }.

setup0() ->
    ok = mnesia:start(),
    {ok, _} = baomi:start_link(lead, ?KEYFILE),
    ok.

cleanup0() ->
    stop_and_purge().

stop_and_purge() ->
    ok = baomi:stop(),
    {atomic, ok} = mnesia:delete_table(?KEYPOOL_TABLE).

test0() ->
    {ok, _SpkiDER} = baomi:get_public_key(),
    ok.

test1() ->
    PublicKey = get_public_key_from_server(),
    {'RSAPublicKey', _, _} = PublicKey,
    ok.

test2() ->
    PrivateKey = get_private_key_from_file(?KEYFILE),
    true = (m1() =:= public_key:decrypt_private(c1(), PrivateKey)),
    true = (m2() =:= public_key:decrypt_private(c2(), PrivateKey)),
    ok.
    
-define(L(X), integer_list_to_binary(X)).
c1() -> ?L([-56, 85, 84, 120, -9, -69, 77, 74, 31, -46, 2, -80, -36,
	    69, 35, 58, 90, -55, 85, -78, -33, 98, -85, 47, 100, -115, 
	    94, -7, 37, 12, -49, -124, -28, -3, -26, 66, 64, 45, 57, 
	    -39, 22, -121, -59, -55, 44, 15, -75, 4, -38, 69, -13, 87, 
	    113, 1, -43, 56, -56, 12, 123, -28, 100, -70, 58, -2]).
c2() -> ?L([39, -50, -111, -25, -85, -16, -83, 96, 99, -93, -42, 118, 
	    108, 78, 89, 2, -93, 33, 53, 17, -26, 77, 68, 69, 69, 42, 
	    28, 30, -83, -92, 83, 115, 127, -89, 74, -40, 122, -116, 
	    18, -70, 33, -58, 38, -103, 52, -18, -110, -70, 62, 93, 126, 
	    31, -56, 14, 1, 63, 31, -50, 91, -104, -92, 71, -25, -86]).
-undef(L).

m1() -> <<1,2,3,4,5,6,7,8,9,0>>.
m2() -> <<77,74,31,5,69,35,58,90,47,100>>.

integer_list_to_binary(L) -> list_to_binary([integer_to_unsigned_byte(X) || X <- L]).
integer_to_unsigned_byte(X) when X < 0 -> X + 256;
integer_to_unsigned_byte(X) when X >= 0 -> X.

test3() ->
    error_logger:tty(false),
    ShortBadKeys = [crypto:rand_bytes(X) || X <- lists:seq(1, ?MINKEYSIZE)],
    IllformedKeys = [crypto:rand_bytes(X) || X <- lists:seq(?MINKEYSIZE, ?MAXKEYSIZE)],
    LongBadKeys = [crypto:rand_bytes(X) || X <- lists:seq(?MAXKEYSIZE + 1, ?MAXKEYSIZE + 10)],
    BadKeys = ShortBadKeys ++ IllformedKeys ++ LongBadKeys,
    [failed = baomi:exchange_key(X) || X <- BadKeys],
    error_logger:tty(true),
    ok.

test4() ->
    PublicKey = get_public_key_from_server(),
    Keys = [crypto:rand_bytes(X) || X <- lists:seq(?MINKEYSIZE, ?MAXKEYSIZE)],
    KeyBlobs = [make_key_blobs(K) || K <- Keys],
    EncryptedKeyBlobs = [public_key:encrypt_public(KB, PublicKey) || {_, _, KB} <- KeyBlobs],
    ExchangeKeyResults = [baomi:exchange_key(Ekb) || Ekb <- EncryptedKeyBlobs],
    ReturnedKeys = [{R, baomi:get_key(KeyId)} || {ok, KeyId, R} <- ExchangeKeyResults],
    RecoveredKeys = [{R, KeyValue} || {R, {ok, KeyValue}} <- ReturnedKeys],
    OriginalKeys = [{R, KeyValue} || {R, KeyValue, _} <- KeyBlobs],
    true = (OriginalKeys == RecoveredKeys),
    ok.
				 
make_key_blobs(Key) ->
    make_key_blobs(Key, <<>>).
make_key_blobs(Key, Padding) ->
    KeySize = length(binary_to_list(Key)),
    Random = crypto:rand_bytes(?KEYRANDOMSIZE),
    {Random, Key, <<?KEYHEADER, KeySize:8, Random/binary, Key/binary, Padding/binary>>}.

get_public_key_from_server() ->
    {ok, SpkiDER} = baomi:get_public_key(),
    public_key:pem_entry_decode({'SubjectPublicKeyInfo', SpkiDER, 0}).

test5() ->
    stop_and_purge(),
    {ok, _} = start_link(lead, ?KEYFILE, [{capacity, 10}]),
    {_, _, EncryptedKeyBlobs, _} = exchange_10_keys(),
    error_logger:tty(false),
    failed = baomi:exchange_key(hd(EncryptedKeyBlobs)),
    error_logger:tty(true),
    ok.

test6() ->
    stop_and_purge(),
    {ok, _} = start_link(lead, ?KEYFILE, [{capacity, 10}]),
    {_, _, EncryptedKeyBlobs, _} = exchange_10_keys(),
    ok = stop(),
    {ok, _} = start_link(recover, ?KEYFILE, [{capacity, 11}]),
    {ok, _, _} = baomi:exchange_key(hd(EncryptedKeyBlobs)),
    error_logger:tty(false),
    failed = baomi:exchange_key(hd(EncryptedKeyBlobs)),
    error_logger:tty(true),
    ok.

exchange_10_keys() ->
    PublicKey = get_public_key_from_server(),
    Keys = [crypto:rand_bytes(?MAXKEYSIZE) || _ <- lists:seq(1, 10)],
    KeyBlobs = [make_key_blobs(K) || K <- Keys],
    EncryptedKeyBlobs = [public_key:encrypt_public(KB, PublicKey) || {_, _, KB} <- KeyBlobs],
    {Keys, KeyBlobs, EncryptedKeyBlobs, [{ok, _, _} = baomi:exchange_key(Ekb) || Ekb <- EncryptedKeyBlobs]}.

test7() ->
    stop_and_purge(),
    {ok, _} = start_link(lead, ?KEYFILE, [{capacity, 10}, {timeout, 1}]),
    {_, _, _, ExchangeKeyResults} = exchange_10_keys(),
    [{ok, _} = baomi:get_key(K) || {ok, K, _} <- ExchangeKeyResults],
    timer:sleep(2000),
    error_logger:tty(false),
    [failed = baomi:get_key(K) || {ok, K, _} <- ExchangeKeyResults],
    error_logger:tty(true),
    exchange_10_keys(),
    ok.

test8() ->    
    stop_and_purge(),
    {ok, _} = start_link(lead, ?KEYFILE, [{capacity, 10}, {timeout, 1}]),
    exchange_10_keys(),
    timer:sleep(2000),
    baomi:purge_expired_keys(),
    timer:sleep(5),
    exchange_10_keys(),
    ok.

-endif.
