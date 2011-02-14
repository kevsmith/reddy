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
    reddy_conn:sync(Conn, ?BLPOP, [Keys, Timeout]);
blpop(Pool, Keys, Timeout) when is_atom(Pool) ->
    ?WITH_POOL(Pool, blpop, [Keys, Timeout]).

brpop(Conn, Keys, Timeout) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?BRPOP, [Keys, Timeout]);
brpop(Pool, Keys, Timeout) when is_atom(Pool) ->
    ?WITH_POOL(Pool, brpop, [Keys, Timeout]).

brpoplpush(Conn, Source, Dest, Timeout) when is_pid(Conn)  ->
    reddy_conn:sync(Conn, ?BRPOPLPUSH, [Source, Dest, Timeout]);
brpoplpush(Pool, Source, Dest, Timeout) when is_atom(Pool) ->
    ?WITH_POOL(Pool, brpoplpush, [Source, Dest, Timeout]).

lindex(Conn, Key, Index) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LINDEX, [Key, Index]);
lindex(Pool, Key, Index) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lindex, [Key, Index]).

lindex_(Conn, Key, Index, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LINDEX, [Key, Index], WantsReturn);
lindex_(Pool, Key, Index, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lindex_, [Key, Index, WantsReturn]).

linsert(Conn, Key, BeforeOrAfter, Pivot, Value) when is_pid(Conn),
                                                     BeforeOrAfter =:= "BEFORE"
                                                     orelse BeforeOrAfter =:= "AFTER" ->
    reddy_conn:sync(Conn, ?LINSERT, [Key, BeforeOrAfter, Pivot, Value]);
linsert(Pool, Key, BeforeOrAfter, Pivot, Value) when is_atom(Pool),
                                                     BeforeOrAfter =:= "BEFORE"
                                                     orelse BeforeOrAfter =:= "AFTER" ->
    ?WITH_POOL(Pool, linsert, [Key, BeforeOrAfter, Pivot, Value]).

linsert_(Conn, Key, BeforeOrAfter, Pivot, Value, WantsReturn) when is_pid(Conn),
                                                                   BeforeOrAfter =:= "BEFORE"
                                                                   orelse BeforeOrAfter =:= "AFTER" ->
    reddy_conn:async(Conn, ?LINSERT, [Key, BeforeOrAfter, Pivot, Value], WantsReturn);
linsert_(Pool, Key, BeforeOrAfter, Pivot, Value, WantsReturn) when is_atom(Pool),
                                                                   BeforeOrAfter =:= "BEFORE"
                                                                   orelse BeforeOrAfter =:= "AFTER" ->
    ?WITH_POOL(Pool, linsert_, [Key, BeforeOrAfter, Pivot, Value, WantsReturn]).

llen(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LLEN, [Key]);
llen(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, llen, [Key]).

llen_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LLEN, [Key], WantsReturn);
llen_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, llen_, [Key, WantsReturn]).

lpop(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LPOP, [Key]);
lpop(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lpop, [Key]).

lpop_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LPOP, [Key], WantsReturn);
lpop_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lpop_, [Key, WantsReturn]).

lpush(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LPUSH, [Key, Value]);
lpush(Pool, Key, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lpush, [Key, Value]).

lpush_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LPUSH, [Key, Value], WantsReturn);
lpush_(Pool, Key, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lpush_, [Key, Value, WantsReturn]).

lpushx(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LPUSHX, [Key, Value]);
lpushx(Pool, Key, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lpushx, [Key, Value]).

lpushx_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LPUSHX, [Key, Value], WantsReturn);
lpushx_(Pool, Key, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lpushx_, [Key, Value, WantsReturn]).

lrange(Conn, Key, Start, Stop) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LRANGE, [Key, Start, Stop]);
lrange(Pool, Key, Start, Stop) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lrange, [Key, Start, Stop]).

lrange_(Conn, Key, Start, Stop, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LRANGE, [Key, Start, Stop], WantsReturn);
lrange_(Pool, Key, Start, Stop, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lrange_, [Key, Start, Stop, WantsReturn]).

lrem(Conn, Key, Count, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LREM, [Key, Count, Value]);
lrem(Pool, Key, Count, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lrem, [Key, Count, Value]).

lrem_(Conn, Key, Count, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LREM, [Key, Count, Value], WantsReturn);
lrem_(Pool, Key, Count, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lrem_, [Key, Count, Value, WantsReturn]).

lset(Conn, Key, Index, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LSET, [Key, Index, Value]);
lset(Pool, Key, Index, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lset, [Key, Index, Value]).

lset_(Conn, Key, Index, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LSET, [Key, Index, Value], WantsReturn);
lset_(Pool, Key, Index, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, lset_, [Key, Index, Value, WantsReturn]).

ltrim(Conn, Key, Start, Stop) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LTRIM, [Key, Start, Stop]);
ltrim(Pool, Key, Start, Stop) when is_atom(Pool) ->
    ?WITH_POOL(Pool, ltrim, [Key, Start, Stop]).

ltrim_(Conn, Key, Start, Stop, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LTRIM, [Key, Start, Stop], WantsReturn);
ltrim_(Pool, Key, Start, Stop, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, ltrim_, [Key, Start, Stop, WantsReturn]).

rpop(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RPOP, [Key]);
rpop(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, rpop, [Key]).

rpop_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?RPOP, [Key], WantsReturn);
rpop_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, rpop_, [Key, WantsReturn]).

rpoplpush(Conn, Source, Dest) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RPOPLPUSH, [Source, Dest]);
rpoplpush(Pool, Source, Dest) when is_atom(Pool) ->
    ?WITH_POOL(Pool, rpoplpush, [Source, Dest]).

rpoplpush_(Conn, Source, Dest, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?RPOPLPUSH, [Source, Dest], WantsReturn);
rpoplpush_(Pool, Source, Dest, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, rpoplpush_, [Source, Dest, WantsReturn]).

rpush(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RPUSH, [Key, Value]);
rpush(Pool, Key, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, rpush, [Key, Value]).

rpush_(Conn, Key, Value, WantsValue) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?RPUSH, [Key, Value], WantsValue);
rpush_(Pool, Key, Value, WantsValue) when is_atom(Pool) ->
    ?WITH_POOL(Pool, rpush_, [Key, Value, WantsValue]).

rpushx(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RPUSHX, [Key, Value]);
rpushx(Pool, Key, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, rpushx, [Key, Value]).

rpushx_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?RPUSHX, [Key, Value], WantsReturn);
rpushx_(Pool, Key, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, rpushx_, [Key, Value, WantsReturn]).
