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

%% List ops
-define(BLPOP, "BLPOP").
-define(BRPOP, "BRPOP").
-define(BRPOPLPUSH, "BRPOPLPUSH").
-define(LINDEX, "LINDEX").
-define(LINSERT, "LINSERT").
-define(LLEN, "LLEN").
-define(LPOP, "LPOP").
-define(LPUSH, "LPUSH").
-define(LPUSHX, "LPUSHX").
-define(LRANGE, "LRANGE").
-define(LREM, "LREM").
-define(LSET, "LSET").
-define(LTRIM, "LTRIM").
-define(RPOP, "RPOP").
-define(RPOPLPUSH, "RPOPLPUSH").
-define(RPUSH, "RPUSH").
-define(RPUSHX, "RPUSHX").

%% Keys ops
-define(DEL, "DEL").
-define(EXISTS, "EXISTS").
-define(EXPIRE, "EXPIRE").
-define(EXPIREAT, "EXPIREAT").
-define(KEYS, "KEYS").
-define(MOVE, "MOVE").
-define(PERSIST, "PERSIST").
-define(RANDOMKEY, "RANDOMKEY").
-define(RENAME, "RENAME").
-define(RENAMENX, "RENAMENX").
-define(TTL, "TTL").
-define(TYPE, "TYPE").

%% Set ops
-define(SADD, "SADD").
-define(SCARD, "SCARD").
-define(SDIFF, "SDIFF").
-define(SDIFFSTORE, "SDIFFSTORE").
-define(SINTER, "SINTER").
-define(SINTERSTORE, "SINTERSTORE").
-define(SISMEMBER, "SISMEMBER").
-define(SMEMBERS, "SMEMBERS").
-define(SMOVE, "SMOVE").
-define(SPOP, "SPOP").
-define(SRANDMEMBER, "SRANDMEMBER").
-define(SREM, "SREM").
-define(SUNION, "SUNION").
-define(SUNIONSTORE, "SUNIONSTORE").

%% Hash ops
-define(HDEL, "HDEL").
-define(HEXISTS, "HEXISTS").
-define(HGET, "HGET").
-define(HGETALL, "HGETALL").
-define(HINCRBY, "HINCRBY").
-define(HKEYS, "HKEYS").
-define(HLEN, "HLEN").
-define(HMGET, "HMGET").
-define(HMSET, "HMSET").
-define(HSET, "HSET").
-define(HSETNX, "HSETNX").
-define(HVALS, "HVALS").

%% String ops
-define(APPEND, "APPEND").
-define(DECR, "DECR").
-define(DECRBY, "DECRBY").
-define(GET, "GET").
-define(GETBIT, "GETBIT").
-define(GETRANGE, "GETRANGE").
-define(GETSET, "GETSET").
-define(INCR, "INCR").
-define(INCRBY, "INCRBY").
-define(MGET, "MGET").
-define(MSET, "MSET").
-define(MSETNX, "MSETNX").
-define(SET, "SET").
-define(SETBIT, "SETBIT").
-define(SETEX, "SETEX").
-define(SETNX, "SETNX").
-define(SETRANGE, "SETRANGE").
-define(STRLEN, "STRLEN").

%% Server ops
-define(INFO, "INFO").
-define(AUTH, "AUTH").

%% Health ops
-define(PING, "PING").
-define(ECHO, "ECHO").

%% Pool macros
-define(WITH_POOL(Pool, Fun, Args), fun() -> reddy_pool:with_pool(Pool, ?MODULE, Fun, Args) end()).
