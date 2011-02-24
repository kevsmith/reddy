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

-module(reddy_protocol).

-include("reddy.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(LTOSTRB(X), integer_to_list(size(X))).
-define(LTOSTR(X), integer_to_list(length(X))).
-define(BTOI(X), list_to_integer(binary_to_list(X))).

-export([parse_status/1,
         parse_integer/1,
         parse_bulk_size/1,
         parse_multi_bulk_count/1]).
-export([to_iolist/1, to_binary/1]).

to_iolist(#reddy_op{name=Op, args=Args}) ->
    TotalSize = length(Args) + 1,
    EncodedArgs = [encode_arg(Arg) || Arg <- Args],
    [encode_op(Op, TotalSize)|EncodedArgs].

to_binary(Op) when is_record(Op, reddy_op) ->
    iolist_to_binary(to_iolist(Op)).

%% Internal functions
encode_arg(Arg) when is_integer(Arg) ->
    Arg1 = integer_to_list(Arg),
    Size = ?LTOSTR(Arg1),
    ["\$", Size, ?REDDY_EOL, Arg1, ?REDDY_EOL];
encode_arg(Arg) when is_float(Arg) ->
    [Arg1] = io_lib:format("~w", Arg),
    Size = ?LTOSTR(Arg1),
    ["\$", Size, ?REDDY_EOL, Arg1, ?REDDY_EOL];
encode_arg(Arg) when is_binary(Arg) ->
    Size = ?LTOSTRB(Arg),
    ["\$", Size, ?REDDY_EOL, Arg, ?REDDY_EOL];
encode_arg(Arg) when is_list(Arg) ->
    Size = ?LTOSTR(Arg),
    ["\$", Size, ?REDDY_EOL, Arg, ?REDDY_EOL].

encode_op(Op, ArgCount) ->
    OpSize = ?LTOSTR(Op),
    ["*", integer_to_list(ArgCount), ?REDDY_EOL, "\$", OpSize, ?REDDY_EOL, Op, ?REDDY_EOL].

parse_status(<<"+OK">>) ->
    ok;
parse_status(<<"+PONG">>) ->
    ok;
parse_status(<<"+", Reason/binary>>) ->
    {ok, Reason};
parse_status(<<"-", Reason/binary>>) ->
    {error, Reason}.

parse_integer(<<":", Number/binary>>) ->
    ?BTOI(Number);
parse_integer(<<"$-1">>) ->
    0;
parse_integer(<<"-ERR ", Reason/binary>>) ->
    {error, Reason}.

parse_bulk_size(<<"\$", Number/binary>>) ->
    ?BTOI(Number);
parse_bulk_size(<<"-ERR ", Reason/binary>>) ->
    {error, Reason}.

parse_multi_bulk_count(<<"*", Number/binary>>) ->
    ?BTOI(Number);
parse_multi_bulk_count(<<"-ERR ", Reason/binary>>) ->
    {error, Reason}.

-ifdef(TEST).
status_test() ->
    [?assertMatch(ok, parse_status(<<"+OK">>)),
     ?assertMatch(ok, parse_status(<<"+PONG">>)),
     ?assertMatch({ok, <<"string">>}, parse_status(<<"+string">>)),
     ?assertMatch({error, <<"BADAUTH">>}, parse_status(<<"-BADAUTH">>)),
     ?assertError(function_clause, parse_status(<<"\$4">>)),
     ?assertError(function_clause, parse_status(<<"\$3">>))].

integer_test() ->
    [?assertMatch(100, parse_integer(<<":100">>)),
     ?assertMatch(-1, parse_integer(<<":-1">>)),
     ?assertMatch(0, parse_integer(<<"$-1">>)),
     ?assertError(function_clause, parse_integer(<<"+OK">>)),
     ?assertError(function_clause, parse_integer(<<"-ERR">>)),
     ?assertError(function_clause, parse_integer(<<"\$3">>))].

bulk_size_test() ->
    [?assertMatch(3, parse_bulk_size(<<"\$3">>)),
     ?assertMatch(-1, parse_bulk_size(<<"\$-1">>)),
     ?assertError(function_clause, parse_bulk_size(<<"+OK">>)),
     ?assertError(function_clause, parse_bulk_size(<<"-ERR">>)),
     ?assertError(function_clause, parse_bulk_size(<<":100">>)),
     ?assertError(function_clause, parse_bulk_size(<<"*22">>))].

multi_bulk_test() ->
    [?assertMatch(1033, parse_multi_bulk_count(<<"*1033">>)),
     ?assertMatch(-1, parse_multi_bulk_count(<<"*-1">>)),
     ?assertError(function_clause, parse_multi_bulk_count(<<"+OK">>)),
     ?assertError(function_clause, parse_multi_bulk_count(<<"-ERR">>)),
     ?assertError(function_clause, parse_multi_bulk_count(<<":100">>)),
     ?assertError(function_clause, parse_multi_bulk_count(<<"\$22">>))].

to_binary_test() ->
    ?assertMatch(<<"*3\r\n$3\r\nSET\r\n$5\r\nmykey\r\n$7\r\nmyvalue\r\n">>,
                 to_binary(#reddy_op{name="SET",
                                     args=[<<"mykey">>, <<"myvalue">>]})).

-endif.
