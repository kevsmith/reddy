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

-module(reddy_strings).

-include("reddy_ops.hrl").

-export([append/3,
         append_/4,
         decr/2,
         decr_/3,
         decrby/3,
         decrby_/4,
         get/2,
         get_/3,
         getbit/3,
         getbit_/4,
         getrange/4,
         getrange_/5,
         getset/3,
         getset_/4,
         incr/2,
         incr_/3,
         incrby/3,
         incrby_/4,
         mget/2,
         mget_/3,
         mset/2,
         mset_/3,
         msetnx/2,
         msetnx_/3,
         set/3,
         set_/4,
         setbit/4,
         setbit_/5,
         setex/4,
         setex_/5,
         setnx/3,
         setnx_/4,
         setrange/4,
         setrange_/5,
         strlen/2,
         strlen_/3]).

append(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?APPEND, [Key, Value]);
append(Pool, Key, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, append, [Key, Value]).

append_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?APPEND, [Key, Value], WantsReturn);
append_(Pool, Key, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, append_, [Key, Value, WantsReturn]).

decr(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?DECR, [Key]);
decr(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, decr, [Key]).

decr_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?DECR, [Key], WantsReturn);
decr_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, decr_, [Key, WantsReturn]).

decrby(Conn, Key, Decrement) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?DECRBY, [Key, Decrement]);
decrby(Pool, Key, Decrement) when is_atom(Pool) ->
    ?WITH_POOL(Pool, decrby, [Key, Decrement]).

decrby_(Conn, Key, Decrement, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?DECRBY, [Key, Decrement], WantsReturn);
decrby_(Pool, Key, Decrement, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, decrby_, [Key, Decrement, WantsReturn]).

get(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?GET, [Key]);
get(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, get, [Key]).

get_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?GET, [Key], WantsReturn);
get_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, get_, [Key, WantsReturn]).

getbit(Conn, Key, Offset) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?GETBIT, [Key, Offset]);
getbit(Pool, Key, Offset) when is_atom(Pool) ->
    ?WITH_POOL(Pool, getbit, [Key, Offset]).

getbit_(Conn, Key, Offset, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?GETBIT, [Key, Offset], WantsReturn);
getbit_(Pool, Key, Offset, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, getbit_, [Key, Offset, WantsReturn]).

getrange(Conn, Key, Start, End) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?GETRANGE, [Key, Start, End]);
getrange(Pool, Key, Start, End) when is_atom(Pool) ->
    ?WITH_POOL(Pool, getrange, [Key, Start, End]).

getrange_(Conn, Key, Start, End, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?GETRANGE, [Key, Start, End], WantsReturn);
getrange_(Pool, Key, Start, End, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, getrange_, [Key, Start, End, WantsReturn]).

getset(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?GETSET, [Key, Value]);
getset(Pool, Key, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, getset, [Key, Value]).

getset_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?GETSET, [Key, Value], WantsReturn);
getset_(Pool, Key, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, getset_, [Key, Value, WantsReturn]).

incr(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?INCR, [Key]);
incr(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, incr, [Key]).

incr_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?INCR, [Key], WantsReturn);
incr_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, incr_, [Key, WantsReturn]).

incrby(Conn, Key, Increment) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?INCRBY, [Key, Increment]);
incrby(Pool, Key, Increment) when is_atom(Pool) ->
    ?WITH_POOL(Pool, incrby, [Key, Increment]).

incrby_(Conn, Key, Increment, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?INCRBY, [Key, Increment], WantsReturn);
incrby_(Pool, Key, Increment, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, incrby_, [Key, Increment, WantsReturn]).

mget(Conn, Keys) when is_pid(Conn),
                      is_list(Keys) ->
    reddy_conn:sync(Conn, ?MGET, Keys);
mget(Pool, Keys) when is_atom(Pool),
                      is_list(Keys) ->
    ?WITH_POOL(Pool, mget, [Keys]).

mget_(Conn, Keys, WantsReturn) when is_pid(Conn),
                                    is_list(Keys) ->
    reddy_conn:async(Conn, ?MGET, Keys, WantsReturn);
mget_(Pool, Keys, WantsReturn) when is_atom(Pool),
                                    is_list(Keys) ->
    ?WITH_POOL(Pool, mget_, [Keys, WantsReturn]).

mset(Conn, KeyValuePairs) when is_pid(Conn),
                               is_list(KeyValuePairs) ->
    reddy_conn:sync(Conn, ?MSET, reddy_types:convert_field_value_pairs(KeyValuePairs));
mset(Pool, KeyValuePairs) when is_atom(Pool),
                               is_list(KeyValuePairs) ->
    ?WITH_POOL(Pool, mset, [KeyValuePairs]).

mset_(Conn, KeyValuePairs, WantsReturn) when is_pid(Conn),
                                             is_list(KeyValuePairs) ->
    reddy_conn:async(Conn, ?MSET, reddy_types:convert_field_value_pairs(KeyValuePairs), WantsReturn);
mset_(Pool, KeyValuePairs, WantsReturn) when is_atom(Pool),
                                             is_list(KeyValuePairs) ->
    ?WITH_POOL(Pool, mset_, [KeyValuePairs, WantsReturn]).

msetnx(Conn, KeyValuePairs) when is_pid(Conn),
                               is_list(KeyValuePairs) ->
    reddy_conn:sync(Conn, ?MSETNX, reddy_types:convert_field_value_pairs(KeyValuePairs));
msetnx(Pool, KeyValuePairs) when is_atom(Pool),
                               is_list(KeyValuePairs) ->
    ?WITH_POOL(Pool, msetnx, [KeyValuePairs]).


msetnx_(Conn, KeyValuePairs, WantsReturn) when is_pid(Conn),
                                               is_list(KeyValuePairs) ->
    reddy_conn:async(Conn, ?MSETNX, reddy_types:convert_field_value_pairs(KeyValuePairs), WantsReturn);
msetnx_(Pool, KeyValuePairs, WantsReturn) when is_atom(Pool),
                                               is_list(KeyValuePairs) ->
    ?WITH_POOL(Pool, msetnx_, [KeyValuePairs, WantsReturn]).

set(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SET, [Key, Value]);
set(Pool, Key, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, set, [Key, Value]).

set_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SET, [Key, Value], WantsReturn);
set_(Pool, Key, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, set_, [Key, Value, WantsReturn]).

setbit(Conn, Key, Offset, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SETBIT, [Key, Offset, Value]);
setbit(Pool, Key, Offset, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, setbit, [Key, Offset, Value]).

setbit_(Conn, Key, Offset, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SETBIT, [Key, Offset, Value], WantsReturn);
setbit_(Pool, Key, Offset, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, setbit_, [Key, Offset, Value, WantsReturn]).

setex(Conn, Key, Seconds, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SETEX, [Key, Seconds, Value]);
setex(Pool, Key, Seconds, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, setex, [Key, Seconds, Value]).

setex_(Conn, Key, Seconds, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SETEX, [Key, Seconds, Value], WantsReturn);
setex_(Pool, Key, Seconds, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, setex_, [Key, Seconds, Value, WantsReturn]).

setnx(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SETNX, [Key, Value]);
setnx(Pool, Key, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, setnx, [Key, Value]).

setnx_(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SETNX, [Key, Value], WantsReturn);
setnx_(Pool, Key, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, setnx_, [Key, Value, WantsReturn]).

setrange(Conn, Key, Offset, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?SETRANGE, [Key, Offset, Value]);
setrange(Pool, Key, Offset, Value) when is_atom(Pool) ->
    ?WITH_POOL(Pool, setrange, [Key, Offset, Value]).

setrange_(Conn, Key, Offset, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?SETRANGE, [Key, Offset, Value], WantsReturn);
setrange_(Pool, Key, Offset, Value, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, setrange_, [Key, Offset, Value, WantsReturn]).

strlen(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?STRLEN, [Key]);
strlen(Pool, Key) when is_atom(Pool) ->
    ?WITH_POOL(Pool, strlen, [Key]).

strlen_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?STRLEN, [Key], WantsReturn);
strlen_(Pool, Key, WantsReturn) when is_atom(Pool) ->
    ?WITH_POOL(Pool, strlen_, [Key, WantsReturn]).
