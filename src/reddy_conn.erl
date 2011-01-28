-module(reddy_conn).

-behaviour(gen_server).

-include("reddy.hrl").

%% API
-export([connect/2,
         close/1,
         start_link/2,
         sync/3,
         async/3,
         async/4,
         async/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {cmd_queue=queue:new(),
                sock}).

connect(Addr, Port) ->
    reddy_conn_sup:new_conn(Addr, Port).

close(Pid) ->
    gen_server:call(Pid, shutdown, infinity).

start_link(Addr, Port) ->
    gen_server:start_link(?MODULE, [Addr, Port], []).

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

init([Addr, Port]) ->
    case gen_tcp:connect(Addr, Port, [binary, {packet, line}, {active, once}]) of
        {ok, Sock} ->
            {ok, #state{sock=Sock}, 15000};
        Error ->
            {stop, Error}
    end.

handle_call(shutdown, _From, State) ->
    {stop, normal, ok, State};
handle_call({enqueue, Op, Caller, WantsReturn}, _From, #state{sock=Sock, cmd_queue=Queue}=State) ->
    Ref = erlang:make_ref(),
    Bin = reddy_protocol:to_iolist(Op),
    case gen_tcp:send(Sock, Bin) of
        ok ->
            Reply = if
                        WantsReturn ->
                            {ok, Ref};
                        true ->
                            ok
                    end,
            {reply, Reply, State#state{cmd_queue=queue:in({Caller, Ref,
                                                           Op#reddy_op.resp_type, WantsReturn}, Queue)}, 15000};
        Error ->
            {reply, Error, State, 15000}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Data}, #state{cmd_queue=Queue}=State) ->
    inet:setopts(Sock, [{packet, raw}]),
    case queue:is_empty(Queue) of
        true ->
            error_logger:warning_msg("Discarding data: ~p~n", [Data]),
            inet:setopts(Sock, [binary, {active, once}, {packet, line}]),
            {noreply, State, 15000};
        false ->
            Body = strip_return_char(Data),
            {{value, {Caller, Ref, ReturnType, WantsReturn}}, Queue1} = queue:out(Queue),
            Result = parse_response(Sock, ReturnType, Body),
            if
                WantsReturn ->
                    Caller ! {Ref, Result};
                true ->
                    ok
            end,
            inet:setopts(Sock, [binary, {active, once}, {packet, line}]),
            {noreply, State#state{cmd_queue=Queue1}, 15000}
    end;
handle_info({tcp_closed, _Sock}, State) ->
    error_logger:warning_msg("Closing connection due to server termination~n"),
    {stop, normal, State};
handle_info({tcp_error, Error, _Sock}, State) ->
    error_logger:warning_msg("Closing connection due to network error: ~p~n", [Error]),
    {stop, normal, State};

handle_info(timeout, #state{sock=Sock, cmd_queue=Queue}=State) ->
    Ref = erlang:make_ref(),
    Op = reddy_ops:create("PING", []),
    Bin = reddy_protocol:to_iolist(Op),
    case gen_tcp:send(Sock, Bin) of
        ok ->
            {noreply, State#state{cmd_queue=queue:in({self(), Ref, Op#reddy_op.resp_type, false}, Queue)}};
        Error ->
            {stop, Error, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
parse_response(_Sock, status, Data) ->
    reddy_protocol:parse_status(Data);
parse_response(_Sock, integer, Data) ->
    reddy_protocol:parse_integer(Data);
parse_response(Sock, bulk, Data) ->
    inet:setopts(Sock, [{active, false}, {packet, raw}]),
    case reddy_protocol:parse_bulk_size(Data) of
        {error, Reason} ->
            {error, Reason};
        -1 ->
            undefined;
        Size ->
            {ok, Body} = gen_tcp:recv(Sock, Size + 2, 0),
            strip_return_char(Body)
    end;
parse_response(Sock, multi_bulk, Data) ->
    inet:setopts(Sock, [{active, false}, {packet, raw}]),
    Count = reddy_protocol:parse_multi_bulk_count(Data),
    read_multi_bulk_body(Sock, Count, []).

read_multi_bulk_body(_Sock, 0, Accum) ->
    lists:reverse(Accum);
read_multi_bulk_body(Sock, Count, Accum) ->
    inet:setopts(Sock, [{packet, line}]),
    {ok, Data} = gen_tcp:recv(Sock, 0, 0),
    Body = strip_return_char(Data),
    read_multi_bulk_body(Sock, Count - 1, [parse_response(Sock, bulk, Body)|Accum]).

strip_return_char(Data) ->
    BodySize = size(Data) - 2,
    <<Body:BodySize/binary, _/binary>> = Data,
    Body.
