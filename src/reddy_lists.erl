-module(reddy_lists).

-include("reddy_ops.hrl").

-export([lpush/3,
         lpush_a/4,
         lpop/2,
         lpop_a/3,
         ltrim/4,
         ltrim_a/5,
         lrange/4,
         lrange_a/5,
         llen/2,
         llen_a/3]).

lpush(Conn, Key, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LPUSH, [Key, Value]);
lpush(Pool, Key, Value) when is_atom(Pool) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = lpush(Conn, Key, Value),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.

lpush_a(Conn, Key, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LPUSH, [Key, Value], WantsReturn);
lpush_a(Pool, Key, Value, WantsReturn) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = lpush_a(Conn, Key, Value, WantsReturn),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.


lpop(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LPOP, [Key]);
lpop(Pool, Key) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = lpop(Conn, Key),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.

lpop_a(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LPOP, [Key], WantsReturn);
lpop_a(Pool, Key, WantsReturn) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = lpop_a(Conn, Key, WantsReturn),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.

ltrim(Conn, Key, Start, Stop) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LTRIM, [Key, Start, Stop]);
ltrim(Pool, Key, Start, Stop) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = ltrim(Conn, Key, Start, Stop),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.


ltrim_a(Conn, Key, Start, Stop, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LTRIM, [Key, Start, Stop], WantsReturn);
ltrim_a(Pool, Key, Start, Stop, WantsReturn) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = ltrim_a(Conn, Key, Start, Stop, WantsReturn),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.


lrange(Conn, Key, Start, Stop) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LRANGE, [Key, Start, Stop]);
lrange(Pool, Key, Start, Stop) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = lrange(Conn, Key, Start, Stop),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.


lrange_a(Conn, Key, Start, Stop, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LRANGE, [Key, Start, Stop], WantsReturn);
lrange_a(Pool, Key, Start, Stop, WantsReturn) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = lrange_a(Conn, Key, Start, Stop, WantsReturn),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.


llen(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?LLEN, [Key]);
llen(Pool, Key) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = llen(Conn, Key),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.


llen_a(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?LLEN, [Key], WantsReturn);
llen_a(Pool, Key, WantsReturn) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = llen_a(Conn, Key, WantsReturn),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.

