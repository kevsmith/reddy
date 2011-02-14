%% Copyright (c) 2011 Kevin Smith <kevin@hypotheticalabs.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

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

sadd(Conn, Key, Member) when is_pid(Conn) ->
  reddy_conn:sync(Conn, ?SADD, [Key, Member]);
sadd(Pool, Key, Member) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sadd, [Key, Member]).

sadd_(Conn, Key, Member, WantsReturn) when is_pid(Conn) ->
  reddy_conn:async(Conn, ?SADD, [Key, Member], WantsReturn);
sadd_(Pool, Key, Member, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sadd_, [Key, Member, WantsReturn]).

scard(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SCARD, [Key]);
scard(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, scard, [Key]).

scard_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SCARD, [Key], WantsReturn);
scard_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, scard_, [Key, WantsReturn]).

sdiff(Conn, Keys) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SDIFF, Keys);
sdiff(Pool, Keys) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sdiff, [Keys]).

sdiff_(Conn, Keys, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SDIFF, Keys, WantsReturn);
sdiff_(Pool, Keys, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sdiff_, [Keys, WantsReturn]).

sdiffstore(Conn, Dest, Keys) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SDIFFSTORE, [Dest, Keys]);
sdiffstore(Pool, Dest, Keys) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sdiffstore, [Dest, Keys]).

sdiffstore_(Conn, Dest, Keys, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SDIFFSTORE, [Dest, Keys], WantsReturn);
sdiffstore_(Pool, Dest, Keys, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sdiffstore_, [Dest, Keys, WantsReturn]).

sinter(Conn, Keys) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SINTER, Keys);
sinter(Pool, Keys) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sinter, [Keys]).

sinter_(Conn, Keys, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SINTER, Keys, WantsReturn);
sinter_(Pool, Keys, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sinter_, [Keys, WantsReturn]).

sinterstore(Conn, Dest, Keys) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SINTERSTORE, [Dest, Keys]);
sinterstore(Pool, Dest, Keys) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sinterstore, [Dest, Keys]).

sinterstore_(Conn, Dest, Keys, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SINTERSTORE, [Dest, Keys], WantsReturn);
sinterstore_(Pool, Dest, Keys, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sinterstore_, [Dest, Keys, WantsReturn]).

sismember(Conn, Key, Member) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SISMEMBER, [Key, Member]);
sismember(Pool, Key, Member) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sismember, [Key, Member]).

sismember_(Conn, Key, Member, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SISMEMBER, [Key, Member], WantsReturn);
sismember_(Pool, Key, Member, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sismember_, [Key, Member, WantsReturn]).

smembers(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SMEMBERS, [Key]);
smembers(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, smembers, [Key]).

smembers_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SMEMBERS, [Key], WantsReturn);
smembers_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, smembers_, [Key, WantsReturn]).

smove(Conn, Source, Dest, Member) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SMOVE, [Source, Dest, Member]);
smove(Pool, Source, Dest, Member) when is_atom(Pool) ->
    ?WITH_POOL(Pool, smove, [Source, Dest, Member]).

smove_(Conn, Source, Dest, Member, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SMOVE, [Source, Dest, Member], WantsReturn);
smove_(Pool, Source, Dest, Member, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, smove_, [Source, Dest, Member, WantsReturn]).

spop(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SPOP, [Key]);
spop(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, spop, [Key]).

spop_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SPOP, [Key], WantsReturn);
spop_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, spop_, [Key, WantsReturn]).

srandmember(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SRANDMEMBER, [Key]);
srandmember(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, srandmember, [Key]).

srandmember_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SRANDMEMBER, [Key], WantsReturn);
srandmember_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, srandmember_, [Key, WantsReturn]).

srem(Conn, Key, Member) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SREM, [Key, Member]);
srem(Pool, Key, Member) when is_atom(Pool) ->
    ?WITH_POOL(Pool, srem, [Key, Member]).

srem_(Conn, Key, Member, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SREM, [Key, Member], WantsReturn);
srem_(Pool, Key, Member, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, srem_, [Key, Member, WantsReturn]).

sunion(Conn, Keys) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SUNION, Keys);
sunion(Pool, Keys) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sunion, Keys).

sunion_(Conn, Keys, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SUNION, Keys, WantsReturn);
sunion_(Pool, Keys, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sunion_, [Keys, WantsReturn]).

sunionstore(Conn, Dest, Keys) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SUNIONSTORE, [Dest, Keys]);
sunionstore(Pool, Dest, Keys) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sunionstore, [Dest, Keys]).

sunionstore_(Conn, Dest, Keys, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SUNIONSTORE, [Dest, Keys], WantsReturn);
sunionstore_(Pool, Dest, Keys, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, sunionstore_, [Dest, Keys, WantsReturn]).
