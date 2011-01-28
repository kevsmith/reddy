-module(reddy_server).

-include("reddy_ops.hrl").

-export([info/1,
         auth/2]).

auth(Conn, Password) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?AUTH, [Password]).


info(Conn) when is_pid(Conn) ->
    Info = binary_to_list(reddy_conn:sync(Conn, ?INFO, [])),
    Lines = string:tokens(Info, "\r\n"),
    [list_to_tuple(string:tokens(Line, ":")) || Line <- Lines];
info(Pool) ->
    case reddy_pool:check_out(Pool) of
        {ok, Conn} ->
            Result = info(Conn),
            reddy_pool:check_in(Pool),
            Result;
        Error ->
            Error
    end.
