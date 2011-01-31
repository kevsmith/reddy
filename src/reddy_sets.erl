-module(reddy_sets).

-include("reddy_ops.hrl").

-export([sadd/3,
         sadd_/4,
         scard/2,
         scard_/3,
         sdiff/2,
         sdiff_/3,
         sdiffstore/3,
         sdiffstore_/4,
         sinter/2,
         sinter_/3,
         sinterstore/3,
         sinterstore_/4,
         sismember/3,
         sismember_/4,
         smembers/2,
         smembers_/3,
         smove/4,
         smove_/5,
         spop/2,
         spop_/3,
         srandmember/2,
         srandmember_/3,
         srem/3,
         srem_/4,
         sunion/2,
         sunion_/3,
         sunionstore/3,
         sunionstore_/4]).

sadd(Conn, Key, Member) ->
  reddy_conn:sync(Conn, ?SADD, [Key, Member]).

sadd_(Conn, Key, Member, WantsReturn) ->
  reddy_conn:async(Conn, ?SADD, [Key, Member], WantsReturn).

scard(Conn, Key) ->
    reddy_conn:sync(Conn, ?SCARD, [Key]).

scard_(Conn, Key, WantsReturn) ->
    reddy_conn:async(Conn, ?SCARD, [Key], WantsReturn).

sdiff(Conn, Keys) ->
    reddy_conn:sync(Conn, ?SDIFF, Keys).

sdiff_(Conn, Keys, WantsReturn) ->
    reddy_conn:async(Conn, ?SDIFF, Keys, WantsReturn).

sdiffstore(Conn, Dest, Keys) ->
    reddy_conn:sync(Conn, ?SDIFFSTORE, [Dest, Keys]).

sdiffstore_(Conn, Dest, Keys, WantsReturn) ->
    reddy_conn:async(Conn, ?SDIFFSTORE, [Dest, Keys], WantsReturn).

sinter(Conn, Keys) ->
    reddy_conn:sync(Conn, ?SINTER, Keys).

sinter_(Conn, Keys, WantsReturn) ->
    reddy_conn:async(Conn, ?SINTER, Keys, WantsReturn).

sinterstore(Conn, Dest, Keys) ->
    reddy_conn:sync(Conn, ?SINTERSTORE, [Dest, Keys]).

sinterstore_(Conn, Dest, Keys, WantsReturn) ->
    reddy_conn:async(Conn, ?SINTERSTORE, [Dest, Keys], WantsReturn).

sismember(Conn, Key, Member) ->
    reddy_conn:sync(Conn, ?SISMEMBER, [Key, Member]).

sismember_(Conn, Key, Member, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SISMEMBER, [Key, Member], WantsReturn).

smembers(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SMEMBERS, [Key]).

smembers_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SMEMBERS, [Key], WantsReturn).

smove(Conn, Source, Dest, Member) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SMOVE, [Source, Dest, Member]).

smove_(Conn, Source, Dest, Member, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SMOVE, [Source, Dest, Member], WantsReturn).

spop(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SPOP, [Key]).

spop_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SPOP, [Key], WantsReturn).

srandmember(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SRANDMEMBER, [Key]).

srandmember_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SRANDMEMBER, [Key], WantsReturn).

srem(Conn, Key, Member) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SREM, [Key, Member]).

srem_(Conn, Key, Member, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SREM, [Key, Member], WantsReturn).

sunion(Conn, Keys) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SUNION, Keys).

sunion_(Conn, Keys, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SUNION, Keys, WantsReturn).

sunionstore(Conn, Dest, Keys) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SUNIONSTORE, [Dest, Keys]).

sunionstore_(Conn, Dest, Keys, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SUNIONSTORE, [Dest, Keys], WantsReturn).
