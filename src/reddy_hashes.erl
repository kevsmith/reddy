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

-module(reddy_hashes).

-include("reddy_ops.hrl").

-export([hdel/3,
         hdel_/4,
         hexists/3,
         hexists_/4,
         hget/3,
         hget_/4,
         hgetall/2,
         hgetall_/3,
         hincrby/4,
         hincrby_/5,
         hkeys/2,
         hkeys_/3,
         hlen/2,
         hlen_/3,
         hmget/3,
         hmget_/4,
         hmset/3,
         hmset_/4,
         hset/4,
         hset_/5,
         hsetnx/4,
         hsetnx_/5,
         hvals/2,
         hvals_/3]).

hdel(Conn, Key, Field) when is_pid(Conn) ->
  reddy_conn:sync(Conn, ?HDEL, [Key, Field]);
hdel(Pool, Key, Field) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hdel, [Key, Field]).

hdel_(Conn, Key, Field, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HDEL, [Key, Field], WantsReturn);
hdel_(Pool, Key, Field, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hdel_, [Key, Field, WantsReturn]).

hexists(Conn, Key, Field) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HEXISTS, [Key, Field]);
hexists(Pool, Key, Field) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hexists, [Key, Field]).

hexists_(Conn, Key, Field, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HEXISTS, [Key, Field], WantsReturn);
hexists_(Pool, Key, Field, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hexists_, [Key, Field, WantsReturn]).

hget(Conn, Key, Field) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HGET, [Key, Field]);
hget(Pool, Key, Field) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hget, [Key, Field]).

hget_(Conn, Key, Field, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HGET, [Key, Field], WantsReturn);
hget_(Pool, Key, Field, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hget_, [Key, Field, WantsReturn]).

hgetall(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HGETALL, [Key]);
hgetall(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hgetall, [Key]).

hgetall_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HGETALL, [Key], WantsReturn);
hgetall_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hgetall_, [Key, WantsReturn]).

hincrby(Conn, Key, Field, Increment) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HINCRBY, [Key, Field, Increment]);
hincrby(Pool, Key, Field, Increment) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hincrby, [Key, Field, Increment]).

hincrby_(Conn, Key, Field, Increment, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HINCRBY, [Key, Field, Increment], WantsReturn);
hincrby_(Pool, Key, Field, Increment, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hincrby_, [Key, Field, Increment, WantsReturn]).

hkeys(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HKEYS, [Key]);
hkeys(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hkeys, [Key]).

hkeys_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HKEYS, [Key], WantsReturn);
hkeys_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hkeys_, [Key, WantsReturn]).

hlen(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HLEN, [Key]);
hlen(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hlen, [Key]).

hlen_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HLEN, [Key], WantsReturn);
hlen_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hlen_, [Key, WantsReturn]).

hmget(Conn, Key, Fields) when is_list(Fields),
                              is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HMGET, [Key, Fields]);
hmget(Pool, Key, Fields) when is_list(Fields),
                              is_atom(Pool) ->
    ?WITH_POOL(Pool, hmget, [Key, Fields]).

hmget_(Conn, Key, Fields, WantsReturn) when is_list(Fields),
                                            is_pid(Conn) ->
    reddy_conn:async(Conn, ?HMGET, [Key, Fields], WantsReturn);
hmget_(Pool, Key, Fields, WantsReturn) when is_list(Fields),
                                            is_atom(Pool) ->
    ?WITH_POOL(Pool, hmget_, [Key, Fields, WantsReturn]).

hmset(Conn, Key, FieldValuePairs) when is_list(FieldValuePairs),
                                       is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HMSET, [Key, reddy_types:convert_field_value_pairs(FieldValuePairs)]);
hmset(Pool, Key, FieldValuePairs) when is_list(FieldValuePairs),
                                       is_atom(Pool) ->
    ?WITH_POOL(Pool, hmset, [Key, FieldValuePairs]).

hmset_(Conn, Key, FieldValuePairs, WantsReturn) when is_list(FieldValuePairs),
                                                     is_pid(Conn) ->
    reddy_conn:async(Conn, ?HMSET, [Key, reddy_types:convert_field_value_pairs(FieldValuePairs)], WantsReturn);
hmset_(Pool, Key, FieldValuePairs, WantsReturn) when is_list(FieldValuePairs),
                                                     is_atom(Pool) ->
    ?WITH_POOL(Pool, hmset_, [Key, FieldValuePairs, WantsReturn]).

hset(Conn, Key, Field, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HSET, [Key, Field, Value]);
hset(Pool, Key, Field, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hset, [Key, Field, Value]).

hset_(Conn, Key, Field, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HSET, [Key, Field, Value], WantsReturn);
hset_(Pool, Key, Field, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hset_, [Key, Field, Value, WantsReturn]).

hsetnx(Conn, Key, Field, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HSETNX, [Key, Field, Value]);
hsetnx(Pool, Key, Field, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hsetnx, [Key, Field, Value]).

hsetnx_(Conn, Key, Field, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HSETNX, [Key, Field, Value], WantsReturn);
hsetnx_(Pool, Key, Field, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hsetnx_, [Key, Field, Value, WantsReturn]).

hvals(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HVALS, [Key]);
hvals(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hvals, [Key]).

hvals_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HVALS, [Key], WantsReturn);
hvals_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, hvals_, [Key, WantsReturn]).
