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
