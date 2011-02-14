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
         keys/2,
         keys_/3,
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

del(Conn, Keys) when is_list(Keys), is_pid(Conn) ->
    reddy_conn:sync(Conn, ?DEL, Keys);
del(Pool, Keys) when is_atom(Pool) ->
    ?WITH_POOL(Pool, del, [Keys]).

del_(Conn, Keys, WantsReturn) when is_list(Keys), is_pid(Conn) ->
    reddy_conn:async(Conn, ?DEL, Keys, WantsReturn).

exists(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?EXISTS, [Key]).
exists_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?EXISTS, [Key], WantsReturn).

expire(Conn, Key, Seconds) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?EXPIRE, [Key, Seconds]).

expire_(Conn, Key, Seconds, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?EXPIRE, [Key, Seconds], WantsReturn).

expireat(Conn, Key, Timestamp) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?EXPIREAT, [Key, convert_ts(Timestamp)]).

expireat_(Conn, Key, Timestamp, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?EXPIREAT, [Key, convert_ts(Timestamp)], WantsReturn).

keys(Conn, Pattern) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?KEYS, [Pattern]).

keys_(Conn, Pattern, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?KEYS, [Pattern], WantsReturn).

move(Conn, Key, Db) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?MOVE, [Key, Db]).

move_(Conn, Key, Db, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?MOVE, [Key, Db], WantsReturn).

persist(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?PERSIST, [Key]).

persist_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?PERSIST, [Key], WantsReturn).

randomkey(Conn) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RANDOMKEY, []).

randomkey_(Conn, WantsReturn) when is_pid(Conn) ->
    reddy_conn:asysnc(Conn, ?RANDOMKEY, [], WantsReturn).

rename(Conn, Key, NewKey) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RENAME, [Key, NewKey]).

rename_(Conn, Key, NewKey, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?RENAME, [Key, NewKey], WantsReturn).

renamenx(Conn, Key, NewKey) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?RENAMENX, [Key, NewKey]).

renamenx_(Conn, Key, NewKey, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?RENAMENX, [Key, NewKey], WantsReturn).

ttl(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?TTL, [Key]).

ttl_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?TTL, [Key], WantsReturn).

type(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?TYPE, [Key]).

type_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?TYPE, [Key], WantsReturn).

%% Internal functions
convert_ts(TS={_, _, _}) ->
    reddy_time:now_to_unixts(TS);
convert_ts(TS) ->
    TS.
