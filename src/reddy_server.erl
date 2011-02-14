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
