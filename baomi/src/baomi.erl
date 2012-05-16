-module(baomi).

-export([exchange_key/1, get_public_key/0, get_key/1, purge_expired_keys/0]).

-define(SERVER, baomi_server).
-ifdef(TEST).
-define(S, gen_server).
-else.
-define(S, (fenliu:resolve_server(?SERVER))).
-endif.

exchange_key(EncryptedKeyBlob) ->
    ?S:call(?SERVER, {exchange_key, EncryptedKeyBlob}).

get_public_key() ->
    ?S:call(?SERVER, get_public_key).

get_key(KeyId) ->
    ?S:call(?SERVER, {get_key, KeyId}).

purge_expired_keys() ->
    ?S:cast(?SERVER, purge_expired_keys).

