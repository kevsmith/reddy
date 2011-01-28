-module(reddy_ops).

-include("reddy.hrl").
-include("reddy_ops.hrl").

-export([create/2]).

create(Cmd, Args) when is_binary(Cmd) ->
    create(binary_to_list(Cmd), Args);

%% List ops
create(Op=?LPUSH, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};
create(Op=?LTRIM, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=status};
create(Op=?LRANGE, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=multi_bulk};
create(Op=?LPOP, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=bulk};
create(Op=?LLEN, Args) ->
    #reddy_op{name=Op,
              args=Args,
              resp_type=integer};

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
