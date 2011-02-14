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

-module(reddy_ops).

-include("reddy.hrl").
-include("reddy_ops.hrl").

-export([create/2]).

create(Cmd, Args) when is_binary(Cmd) ->
    create(binary_to_list(Cmd), Args);

%% List ops
create(Op=?BLPOP, Args) ->
    #reddy_op{name=Op,
              args=lists:flatten(Args),
              resp_type=multi_bulk};
create(Op=?BRPOP, Args) ->
    #reddy_op{name=Op,
              args=lists:flatten(Args),
              resp_type=multi_bulk};
create(Op=?BRPOPLPUSH, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};
create(Op=?LINDEX, Args) ->
  #reddy_op{name=Op,
            args=Args,
            resp_type=bulk};
create(Op=?LINSERT, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?LLEN, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?LPOP, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk};
create(Op=?LPUSH, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?LPUSHX, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?LRANGE, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};
create(Op=?LREM, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?LSET, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=status};

create(Op=?LTRIM, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=status};

create(Op=?RPOP, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk};

create(Op=?RPOPLPUSH, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk};

create(Op=?RPUSH, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?RPUSHX, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

%% Key ops
create(Op=?DEL, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?EXISTS, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?EXPIRE, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?EXPIREAT, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?KEYS, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};

create(Op=?MOVE, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?PERSIST, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?RANDOMKEY, _Args) ->
    #reddy_op{name=Op,
              args=[],
              resp_type=bulk};

create(Op=?RENAME, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=status};

create(Op=?RENAMENX, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?TTL, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?TYPE, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=status};

%% Set ops
create(Op=?SADD, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?SCARD, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?SDIFF, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};

create(Op=?SDIFFSTORE, Args) ->
    #reddy_op{name=Op,
              args=lists:flatten(Args),
              resp_type=integer};

create(Op=?SINTER, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};

create(Op=?SINTERSTORE, Args) ->
    #reddy_op{name=Op,
              args=lists:flatten(Args),
              resp_type=integer};

create(Op=?SISMEMBER, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?SMEMBERS, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};

create(Op=?SMOVE, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?SPOP, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk};

create(Op=?SRANDMEMBER, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk};

create(Op=?SREM, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?SUNION, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};

create(Op=?SUNIONSTORE, Args) ->
    #reddy_op{name=Op,
              args=lists:flatten(Args),
              resp_type=integer};

%% Hash ops
create(Op=?HDEL, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?HEXISTS, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?HGET, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?HGETALL, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};

create(Op=?HINCRBY, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?HKEYS, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};

create(Op=?HLEN, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?HMGET, Args) ->
    #reddy_op{name=Op,
              args=lists:flatten(Args),
              resp_type=multi_bulk};

create(Op=?HMSET, Args) ->
    #reddy_op{name=Op,
              args=lists:flatten(Args),
              resp_type=status};

create(Op=?HSET, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?HSETNX, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

create(Op=?HVALS, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};

%% String ops
create(Op=?APPEND, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?DECR, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?DECRBY, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?GET, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk};
create(Op=?GETBIT, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?GETRANGE, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk};
create(Op=?GETSET, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk};
create(Op=?INCR, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?INCRBY, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?MGET, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};
create(Op=?MSETNX, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?SET, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=status};
create(Op=?SETBIT, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?SETEX, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=status};
create(Op=?SETNX, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?SETRANGE, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?STRLEN, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

%% Server ops
create(Op=?INFO, _Args) ->
    #reddy_op{name=Op,
              args=[],
              resp_type=bulk};
create(Op=?AUTH, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=status};

%% Health ops
create(Op=?PING, _Args) ->
    #reddy_op{name=Op,
              args=[],
              resp_type=status};
create(Op=?ECHO, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk}.
