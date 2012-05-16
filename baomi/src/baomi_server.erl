-module(baomi_server).
-author(yong.david.huang@nokia.com).

-behaviour(gen_server).

-include("../../include/defs.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0, start_link/1, start_link/2, start_link/3, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(KEYPOOL_TABLE, baomi_key_pool).

-define(DEFAULT_KEYFILE, "baomi.key").
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

start_link() ->
    Env = application:get_all_env(),
    KeyFile = proplists:get_value(keyfile, Env, "testkey0"),
    Capacity = proplists:get_value(capacity, Env, 1*?MILLION),
    Timeout = proplists:get_value(timeout, Env, 2*?HOURS),
    start_link(KeyFile, Capacity, Timeout).

start_link(KeyFile) ->
    Env = application:get_all_env(),
    Capacity = proplists:get_value(capacity, Env, 1*?MILLION),
    Timeout = proplists:get_value(timeout, Env, 2*?HOURS),
    start_link(KeyFile, Capacity, Timeout).

start_link(KeyFile, Capacity) ->
    Env = application:get_all_env(),
    Timeout = proplists:get_value(timeout, Env, 2*?HOURS),
    start_link(KeyFile, Capacity, Timeout).

start_link(KeyFile, Capacity, Timeout) ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [KeyFile, Capacity, Timeout], []).

stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([KeyFile, Capacity, Timeout]) ->
    error_logger:info_msg(io_lib:format("Baomi: { keyfile = ~s, capacity = ~p, timeout = ~p~n", [KeyFile, Capacity, Timeout])),

    PrivateKey = get_private_key_from_file(KeyFile),
    PublicKey = #'RSAPublicKey'{modulus = PrivateKey#'RSAPrivateKey'.modulus, 
				publicExponent = PrivateKey#'RSAPrivateKey'.publicExponent},

    %% if table exists -> check structure -> if table local? -> done
    %%                                                       -> replicate to local
    %%                 -> create table locally
    case lists:member(?KEYPOOL_TABLE, mnesia:system_info(tables)) of
	true ->
	    R = record_info(fields, key),
	    R = mnesia:table_info(?KEYPOOL_TABLE, attributes),
	    %% if table is local
	    case lists:member(?KEYPOOL_TABLE, mnesia:system_info(local_tables)) of
		true ->
		    ok;
		false ->
		    {atomic, ok} = mnesia:add_table_copy(?KEYPOOL_TABLE, node(), ram_copies),
		    ok = mnesia:wait_for_tables([?KEYPOOL_TABLE], infinity)
	    end;
	false ->
	    {atomic, ok} = mnesia:create_table(?KEYPOOL_TABLE, [{attributes, record_info(fields, key)},
								{ram_copies, [node()]},
								{record_name, key}])
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
		    case now_in_seconds() >= key_expire_time(Key) of
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
    <<KeyValue:KeySize/bytes, _Anything/binary>> = Rest2,
    {Random, KeyValue}.
	    
new_key(KeyValue, Timeout) ->
    Id = crypto:rand_bytes(16),
    ExpireTime = now_in_seconds() + Timeout,
    #key{id = Id, value = KeyValue, expire_time = ExpireTime}.

read_key(KeyId) ->
    F = fun() -> mnesia:read(?KEYPOOL_TABLE, KeyId, read) end,
    {atomic, [Key]} = mnesia:transaction(F),
    Key.

delete_key_if_expired(Key) ->
    case now_in_seconds() >= key_expire_time(Key) of
	true ->
	    F = fun() -> mnesia:delete(?KEYPOOL_TABLE, key_id(Key), write) end,
	    ok = mnesia:transaction(F);
	false ->
	    ok
    end.

now_in_seconds() ->
    Now = calendar:now_to_datetime(now()),
    calendar:datetime_to_gregorian_seconds(Now).    

%%%===================================================================
%%% Unit tests
%%%===================================================================

-ifdef(TEST).

-define(KEYFILE, "../src/testkey1").
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
    ?debugMsg(io_lib:format("pwd =~p", [file:get_cwd()])),
    {ok, _} = start_link(?KEYFILE),
    ok.

cleanup0() ->
    stop_and_purge().

stop_and_purge() ->
    ok = stop(),
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
    true = (m1() =:= public_key:decrypt_private(c1_1024(), PrivateKey)),
    true = (m2() =:= public_key:decrypt_private(c2_1024(), PrivateKey)),
    ok.
    
-define(L(X), integer_list_to_binary(X)).
%c1_512() -> ?L([-56, 85, 84, 120, -9, -69, 77, 74, 31, -46, 2, -80, -36,
%		69, 35, 58, 90, -55, 85, -78, -33, 98, -85, 47, 100, -115, 
%		94, -7, 37, 12, -49, -124, -28, -3, -26, 66, 64, 45, 57, 
%		-39, 22, -121, -59, -55, 44, 15, -75, 4, -38, 69, -13, 87, 
%		113, 1, -43, 56, -56, 12, 123, -28, 100, -70, 58, -2]).
%c2_512() -> ?L([39, -50, -111, -25, -85, -16, -83, 96, 99, -93, -42, 118, 
%	    108, 78, 89, 2, -93, 33, 53, 17, -26, 77, 68, 69, 69, 42, 
%	    28, 30, -83, -92, 83, 115, 127, -89, 74, -40, 122, -116, 
%	    18, -70, 33, -58, 38, -103, 52, -18, -110, -70, 62, 93, 126, 
%	    31, -56, 14, 1, 63, 31, -50, 91, -104, -92, 71, -25, -86]).

c1_1024() -> ?L([-112, -106, 35, -33, -33, -14, 35, -59, 114, 91, 25, 89, 2,
		 101, -49, 18, -124, 103, 17, -47, 16, -39, -65, 27, -126, 
		 -44, -3, 119, -30, 49, -96, -84, 94, 90, -102, 66, -95, -40,
		 -74, -108, 116, -119, 8, -112, -26, 9, 68, 91, -44, -78, 73,
		 69, -82, 120, -43, -95, 51, -45, 114, 127, -71, 66, 28, -88, 
		 -27, -40, 58, -46, 55, -59, 24, 56, -85, -58, -109, 103, -108,
		 65, 9, -109, 116, -85, -112, -59, -39, -8, -60, 66, 35, -86,
		 -43, -42, 15, -42, 126, 105, 98, 15, -22, -59, -60, -23, -15, 
		 111, 93, -95, 27, -67, 27, -77, -125, 62, 83, -65, -77, 58, 
		 83, -116, -30, -26, 50, -126, 105, -113, 99, 42, 49, -117]).
    
c2_1024() -> ?L([23, 24, -67, -66, -15, 94, -21, 121, -82, 12, -58, 103, -15, 
		 94, -8, -102, -33, 60, 123, -10, -12, -108, 62, 11, 25, 84, 5, 
		 -83, 33, 96, -59, -75, -110, 103, 84, 84, 109, 56, -72, -97, 
		 -14, -78, 9, 26, 3, 106, 103, -5, -117, -124, 105, -48, 55, 
		 -50, -103, 19, -72, -69, -81, -26, -109, 76, 55, -33, -63, 
		 -117, -114, -66, 93, -35, 26, 70, 81, -128, 26, -113, -91, 
		 -47, 111, -56, 33, -113, -28, 70, -84, 67, -56, -17, 108, 
		 -74, 64, -15, 36, 96, 116, 1, 99, 19, -8, -93, 43, 76, -15, 
		 51, -83, -40, -4, 14, 26, -26, 56, -40, -10, 46, 66, 98, -98,
		 82, 106, 3, -103, -91, -54, -25, 23, -73, 64, -84]).
    
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
    {ok, _} = start_link(?KEYFILE, 10),
    {_, _, EncryptedKeyBlobs, _} = exchange_10_keys(),
    error_logger:tty(false),
    failed = baomi:exchange_key(hd(EncryptedKeyBlobs)),
    error_logger:tty(true),
    ok.

test6() ->
    stop_and_purge(),
    {ok, _} = start_link(?KEYFILE, 10),
    {_, _, EncryptedKeyBlobs, _} = exchange_10_keys(),
    ok = stop(),
    {ok, _} = start_link(?KEYFILE, 11),
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
    {ok, _} = start_link(?KEYFILE, 10, 1),
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
    {ok, _} = start_link(?KEYFILE, 10, 1),
    exchange_10_keys(),
    timer:sleep(2000),
    baomi:purge_expired_keys(),
    timer:sleep(5),
    exchange_10_keys(),
    ok.

-endif.
