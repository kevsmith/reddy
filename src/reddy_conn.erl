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

-module(reddy_conn).

-behaviour(gen_server).

-include("reddy.hrl").

%% API
-export([connect/2,
         connect/3,
         close/1,
         start_link/4,
         sync/3,
         async/3,
         async/4,
         async/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {cmd_queue=queue:new(),
                tracefile,
                sock}).

connect(Addr, Port) ->
    connect(Addr, Port, []).

connect(Addr, Port, Opts) ->
    reddy_conn_sup:new_conn(self(), Addr, Port, Opts).

close(Pid) ->
    gen_server:call(Pid, shutdown, infinity).

start_link(Owner, Addr, Port, Opts) ->
    gen_server:start_link(?MODULE, [Owner, Addr, Port, Opts], []).

sync(Pid, Cmd, Args) ->
    case async(Pid, Cmd, Args) of
        {ok, Ref} ->
            receive
                {Ref, Result} ->
                    Result
            end;
        Error ->
            Error
    end.

async(Pid, Cmd, Args) ->
    async(Pid, Cmd, Args, true).

async(Pid, Cmd, Args, WantsReturn) ->
    async(Pid, self(), Cmd, Args, WantsReturn).

async(Pid, Caller, Cmd, Args, WantsReturn) ->
    Op = reddy_ops:create(Cmd, Args),
    case catch gen_server:call(Pid, {enqueue, Op, Caller, WantsReturn}, infinity) of
        {'EXIT', {noproc, _, _}} ->
            {error, closed};
        Result ->
            Result
    end.

%%===============================================
%% gen_server callbacks
%%===============================================

init([Owner, Addr, Port, Opts]) ->
    case gen_tcp:connect(Addr, Port, [binary, {packet, line}, {active, once}]) of
        {ok, Sock} ->
            erlang:monitor(process, Owner),
            case proplists:get_value(trace_file, Opts) of
                undefined ->
                    {ok, #state{sock=Sock}};
                Path ->
                    {ok, #state{sock=Sock, tracefile=Path}}
            end;
        Error ->
            {stop, Error}
    end.

handle_call(shutdown, _From, State) ->
    {stop, normal, ok, State};
handle_call({enqueue, Op, Caller, WantsReturn}, _From, #state{sock=Sock, cmd_queue=Queue,
                                                              tracefile=File}=State) ->
    Ref = erlang:make_ref(),
    Bin = reddy_protocol:to_iolist(Op),
    log_client(File, Bin),
    case gen_tcp:send(Sock, Bin) of
        ok ->
            Reply = if
                        WantsReturn ->
                            {ok, Ref};
                        true ->
                            ok
                    end,
            {reply, Reply, State#state{cmd_queue=queue:in({Caller, Ref,
                                                           Op#reddy_op.resp_type, WantsReturn}, Queue)}};
        Error ->
            {reply, Error, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, _Type, _Object, _Info}, State) ->
    {stop, normal, State};
handle_info({tcp, Sock, Data}, #state{cmd_queue=Queue, tracefile=File}=State) ->
    log_server(File, Data),
    inet:setopts(Sock, [{packet, raw}]),
    case queue:is_empty(Queue) of
        true ->
            error_logger:warning_msg("Discarding data: ~p~n", [Data]),
            inet:setopts(Sock, [binary, {active, once}, {packet, line}]),
            {noreply, State};
        false ->
            Body = strip_return_char(Data),
            {{value, {Caller, Ref, ReturnType, WantsReturn}}, Queue1} = queue:out(Queue),
            Result = parse_response(Sock, File, ReturnType, Body),
            if
                WantsReturn ->
                    Caller ! {Ref, Result};
                true ->
                    ok
            end,
            inet:setopts(Sock, [binary, {active, once}, {packet, line}]),
            {noreply, State#state{cmd_queue=Queue1}}
    end;
handle_info({tcp_closed, _Sock}, State) ->
    error_logger:warning_msg("Closing connection due to server termination~n"),
    {stop, normal, State};
handle_info({tcp_error, Error, _Sock}, State) ->
    error_logger:warning_msg("Closing connection due to network error: ~p~n", [Error]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
parse_response(_Sock, _File, status, Data) ->
    reddy_protocol:parse_status(Data);
parse_response(_Sock, _File, integer, Data) ->
    reddy_protocol:parse_integer(Data);
parse_response(Sock, File, bulk, Data) ->
    inet:setopts(Sock, [{active, false}, {packet, raw}]),
    case reddy_protocol:parse_bulk_size(Data) of
        {error, Reason} ->
            {error, Reason};
        -1 ->
            undefined;
        Size ->
            {ok, Body} = gen_tcp:recv(Sock, Size + 2, 0),
            log_server(File, Body),
            strip_return_char(Body)
    end;
parse_response(Sock, File, multi_bulk, Data) ->
    inet:setopts(Sock, [{active, false}, {packet, raw}]),
    case reddy_protocol:parse_multi_bulk_count(Data) of
        -1 ->
            undefined;
        Count ->
            read_multi_bulk_body(Sock, File, Count, [])
    end.

read_multi_bulk_body(_Sock, _File, 0, Accum) ->
    lists:reverse(Accum);
read_multi_bulk_body(Sock, File, Count, Accum) ->
    inet:setopts(Sock, [{packet, line}]),
    {ok, Data} = gen_tcp:recv(Sock, 0, 0),
    log_server(File, Data),
    Body = strip_return_char(Data),
    read_multi_bulk_body(Sock, File, Count - 1, [parse_response(Sock, File, bulk, Body)|Accum]).

strip_return_char(Data) ->
    BodySize = size(Data) - 2,
    <<Body:BodySize/binary, _/binary>> = Data,
    Body.

log_server(undefined, _Output) ->
    ok;
log_server(File, Output) ->
    file:write_file(File, ["-----SERVER-----\n", Output], [append]).

log_client(undefined, _Output) ->
    ok;
log_client(File, Output) ->
    file:write_file(File, ["-----CLIENT-----\n", Output], [append]).
