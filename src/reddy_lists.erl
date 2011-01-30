-module(reddy_lists).

-include("reddy_ops.hrl").

-export([blpop/3,
         brpop/3,
         brpoplpush/4,
         lindex/3,
         lindex_/4,
         linsert/5,
         linsert_/6,
         llen/2,
         llen_/3,
         lpop/2,
         lpop_/3,
         lpush/3,
         lpush_/4,
         lpushx/3,
         lpushx_/4,
         lrange/4,
         lrange_/5,
         lrem/4,
         lrem_/5,
         lset/4,
         lset_/5,
         ltrim/4,
         ltrim_/5,
         rpop/2,
         rpop_/3,
         rpoplpush/3,
         rpoplpush_/4,
         rpush/3,
         rpush_/4,
         rpushx/3,
         rpushx_/4]).

blpop(Conn, Keys, Timeout) when is_pid(Conn) ->
  reddy_conn:sync(Conn, ?BLPOP, [Keys, Timeout]).

brpop(Conn, Keys, Timeout) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?BRPOP, [Keys, Timeout]).

brpoplpush(Conn, Source, Dest, Timeout) ->
    reddy_conn:sync(Conn, ?BRPOPLPUSH, [Source, Dest, Timeout]).

lindex(Conn, Key, Index) when is_pid(Conn) ->
  reddy_conn:sync(Conn, ?LINDEX, [Key, Index]).

lindex_(Conn, Key, Index, WantsReturn) ->
  reddy_conn:async(Conn, ?LINDEX, [Key, Index], WantsReturn).

linsert(Conn, Key, BeforeOrAfter, Pivot, Value) when is_pid(Conn),
                                                     BeforeOrAfter =:= "BEFORE"
                                                     orelse BeforeOrAfter =:= "AFTER" ->
  reddy_conn:sync(Conn, ?LINSERT, [Key, BeforeOrAfter, Pivot, Value]).

linsert_(Conn, Key, BeforeOrAfter, Pivot, Value, WantsReturn) when is_pid(Conn),
                                                                   BeforeOrAfter =:= "BEFORE"
                                                                   orelse BeforeOrAfter =:= "AFTER" ->
    reddy_conn:async(Conn, ?LINSERT, [Key, BeforeOrAfter, Pivot, Value], WantsReturn).

llen(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LLEN, [Key]).

llen_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LLEN, [Key], WantsReturn).

lpop(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LPOP, [Key]).

lpop_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LPOP, [Key], WantsReturn).

lpush(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LPUSH, [Key, Value]).

lpush_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LPUSH, [Key, Value], WantsReturn).

lpushx(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LPUSHX, [Key, Value]).

lpushx_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LPUSHX, [Key, Value], WantsReturn).

lrange(Conn, Key, Start, Stop) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LRANGE, [Key, Start, Stop]).

lrange_(Conn, Key, Start, Stop, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LRANGE, [Key, Start, Stop], WantsReturn).

lrem(Conn, Key, Count, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LREM, [Key, Count, Value]).

lrem_(Conn, Key, Count, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LREM, [Key, Count, Value], WantsReturn).

lset(Conn, Key, Index, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LSET, [Key, Index, Value]).

lset_(Conn, Key, Index, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LSET, [Key, Index, Value], WantsReturn).

ltrim(Conn, Key, Start, Stop) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LTRIM, [Key, Start, Stop]).

ltrim_(Conn, Key, Start, Stop, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LTRIM, [Key, Start, Stop], WantsReturn).

rpop(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RPOP, [Key]).

rpop_(Conn, Key, WantsReturn) ->
    reddy_conn:async(Conn, ?RPOP, [Key], WantsReturn).

rpoplpush(Conn, Source, Dest) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RPOPLPUSH, [Source, Dest]).

rpoplpush_(Conn, Source, Dest, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?RPOPLPUSH, [Source, Dest], WantsReturn).

rpush(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RPUSH, [Key, Value]).

rpush_(Conn, Key, Value, WantsValue) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?RPUSH, [Key, Value], WantsValue).

rpushx(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RPUSHX, [Key, Value]).

rpushx_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?RPUSHX, [Key, Value], WantsReturn).
