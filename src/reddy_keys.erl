-module(reddy_keys).

-include("reddy_ops.hrl").

-export([del/2,
         del_/3,
         exists/2,
         exists_/3,
         expire/3,
         expire_/4,
         expireat/3,
         expireat_/4,
         keys/1,
         keys_/2,
         move/3,
         move_/4,
         persist/2,
         persist_/3,
         randomkey/1,
         randomkey_/2,
         rename/3,
         rename_/4,
         renamenx/3,
         renamenx_/4,
         ttl/2,
         ttl_/3,
         type/2,
         type_/3]).

del(Conn, Keys) when is_list(Keys) ->
    reddy_conn:sync(Conn, ?DEL, Keys).

del_(Conn, Keys, WantsReturn) when is_list(Keys) ->
    reddy_conn:async(Conn, ?DEL, Keys, WantsReturn).

exists(Conn, Key) ->
    reddy_conn:sync(Conn, ?EXISTS, [Key]).
exists_(Conn, Key, WantsReturn) ->
    reddy_conn:async(Conn, ?EXISTS, [Key], WantsReturn).

expire(Conn, Key, Seconds) ->
    reddy_conn:sync(Conn, ?EXPIRE, [Key, Seconds]).

expire_(Conn, Key, Seconds, WantsReturn) ->
    reddy_conn:async(Conn, ?EXPIRE, [Key, Seconds], WantsReturn).

expireat(Conn, Key, Timestamp) ->
    reddy_conn:sync(Conn, ?EXPIREAT, [Key, convert_ts(Timestamp)]).

expireat_(Conn, Key, Timestamp, WantsReturn) ->
    reddy_conn:async(Conn, ?EXPIREAT, [Key, convert_ts(Timestamp)], WantsReturn).

keys(Conn) ->
    reddy_conn:sync(Conn, ?KEYS, []).

keys_(Conn, WantsReturn) ->
    reddy_conn:async(Conn, ?KEYS, [], WantsReturn).

move(Conn, Key, Db) ->
    reddy_conn:sync(Conn, ?MOVE, [Key, Db]).

move_(Conn, Key, Db, WantsReturn) ->
    reddy_conn:async(Conn, ?MOVE, [Key, Db], WantsReturn).

persist(Conn, Key) ->
    reddy_conn:sync(Conn, ?PERSIST, [Key]).

persist_(Conn, Key, WantsReturn) ->
    reddy_conn:async(Conn, ?PERSIST, [Key], WantsReturn).

randomkey(Conn) ->
    reddy_conn:sync(Conn, ?RANDOMKEY, []).

randomkey_(Conn, WantsReturn) ->
    reddy_conn:asysnc(Conn, ?RANDOMKEY, [], WantsReturn).

rename(Conn, Key, NewKey) ->
    reddy_conn:sync(Conn, ?RENAME, [Key, NewKey]).

rename_(Conn, Key, NewKey, WantsReturn) ->
    reddy_conn:async(Conn, ?RENAME, [Key, NewKey], WantsReturn).

renamenx(Conn, Key, NewKey) ->
    reddy_conn:sync(Conn, ?RENAMENX, [Key, NewKey]).

renamenx_(Conn, Key, NewKey, WantsReturn) ->
    reddy_conn:async(Conn, ?RENAMENX, [Key, NewKey], WantsReturn).

ttl(Conn, Key) ->
    reddy_conn:sync(Conn, ?TTL, [Key]).

ttl_(Conn, Key, WantsReturn) ->
    reddy_conn:async(Conn, ?TTL, [Key], WantsReturn).

type(Conn, Key) ->
    reddy_conn:sync(Conn, ?TYPE, [Key]).

type_(Conn, Key, WantsReturn) ->
    reddy_conn:async(Conn, ?TYPE, [Key], WantsReturn).

%% Internal functions
convert_ts(TS={_, _, _}) ->
    reddy_time:now_to_unixts(TS);
convert_ts(TS) ->
    TS.
