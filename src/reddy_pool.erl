-module(reddy_pool).

-behaviour(gen_server).

%% API
-export([start_link/2,
         new_pool/2,
         check_out/1,
         check_in/1]).

-record('DOWN', {mref,
                 type,
                 obj,
                 info}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {addr,
                port,
                pass,
                avail,
                waiting=queue:new(),
                owner_to_child=dict:new(),
                child_to_owner=dict:new()}).

new_pool(PoolName, Options) ->
    reddy_pool_sup:new_pool(PoolName, Options).

check_out(PoolName) ->
    gen_server:call(PoolName, {checkout, self()}, infinity).
check_in(PoolName) ->
    gen_server:call(PoolName, {checkin, self()}, infinity).

start_link(PoolName, Options) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [Options], []).

%%===============================================
%% gen_server callbacks
%%===============================================

init([Options]) ->
    io:format("Options: ~p~n", [Options]),
    Ip = proplists:get_value(ip, Options),
    Port = proplists:get_value(port, Options),
    Pass = proplists:get_value(pass, Options),
    Count = proplists:get_value(count, Options),
    case Ip =:= undefined orelse Port =:= undefined orelse Count =:= undefined of
        true ->
            {stop, {error, badarg}};
        false ->
            case start_children(Ip, Port, Pass, Count, []) of
                {ok, Children} ->
                    {ok, #state{addr=Ip, port=Port, avail=Children}};
                Error ->
                    {stop, Error}
            end
    end.

handle_call({checkout, Owner}, From, #state{avail=[], waiting=Queue}=State) ->
    {noreply, State#state{waiting=queue:in({Owner, From}, Queue)}};
handle_call({checkout, Owner}, _From, #state{avail=[H|T], owner_to_child=Owners,
                                             child_to_owner=Children}=State) ->
    %% Process already has a connection so send it an error
    case dict:find(Owner, Owners) of
        {ok, _} ->
            {reply, {error, already_checked_out}, State};
        error ->
            {Owners1, Children1} = child_out(H, Owner, Owners, Children),
            {reply, {ok, H}, State#state{owner_to_child=Owners1, child_to_owner=Children1, avail=T}}
    end;
handle_call({checkin, Owner}, _From, #state{avail=Avail, owner_to_child=Owners,
                                            child_to_owner=Children}=State) ->
    case dict:find(Owner, Owners) of
        error ->
            {reply, {error, not_checked_out}, State};
        {ok, {Conn, MRef}} ->
            erlang:demonitor(MRef, [flush]),
            Owners1 = dict:erase(Owner, Owners),
            Children1 = dict:erase(Conn, Children),
            State1 = State#state{owner_to_child=Owners1, child_to_owner=Children1,
                                 avail=Avail ++ [Conn]},
            {reply, ok, dequeue_waiting(State1)}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'DOWN'{obj=Pid}, #state{owner_to_child=Owners, child_to_owner=Children}=State) ->
    State1 = case dict:find(Pid, Owners) of
                 error ->
                     case dict:find(Pid, Children) of
                         error ->
                             State;
                         {ok, _} ->
                             dequeue_waiting(crashed_child(Pid, State))
                     end;
                 {ok, _} ->
                     dequeue_waiting(crashed_owner(Pid, State))
             end,
    {noreply, State1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
crashed_child(Pid, #state{addr=Addr, port=Port, avail=Avail, child_to_owner=Children,
                          owner_to_child=Owners}=State) ->
    {Owner, MRef} = dict:fetch(Pid, Children),
    erlang:demonitor(MRef, [flush]),
    Owners1 = dict:erase(Owner, Owners),
    Children1 = dict:erase(Pid, Children),
    case reddy_conn:connect(Addr, Port) of
        {ok, Conn} ->
            State#state{child_to_owner=Children1, owner_to_child=Owners1,
                        avail=Avail ++ [Conn]};
        Error ->
            error_logger:warning_msg("Replacement for crashed redis connection failed: ~p~n", [Error]),
            State#state{child_to_owner=Children1, owner_to_child=Owners1}
    end.

crashed_owner(Pid, #state{avail=Avail, child_to_owner=Children, owner_to_child=Owners}=State) ->
    {Owner, MRef} = dict:fetch(Pid, Children),
    erlang:demonitor(MRef, [flush]),
    Children1 = dict:erase(Pid, Children),
    Owners1 = dict:erase(Owner, Owners),
    State#state{child_to_owner=Children1, owner_to_child=Owners1, avail=Avail ++ [Pid]}.

dequeue_waiting(#state{avail=[H|T], owner_to_child=Owners, child_to_owner=Children,
                       waiting=Queue}=State) ->
    case queue:is_empty(Queue) of
        true ->
            State;
        false ->
            {{value, {Owner, From}}, Queue1} = queue:out(Queue),
            case erlang:is_process_alive(Owner) of
                false ->
                    dequeue_waiting(State#state{waiting=Queue1});
                true ->
                    {Owners1, Children1} = child_out(H, Owner, Owners, Children),
                    gen_server:reply(From, {ok, H}),
                    State#state{owner_to_child=Owners1, child_to_owner=Children1, avail=T, waiting=Queue1}
            end
    end.

child_out(Conn, Owner, Owners, Children) ->
    MRef = erlang:monitor(process, Owner),
    Owners1 = dict:store(Owner, {Conn, MRef}, Owners),
    Children1 = dict:store(Conn, {Owner, MRef}, Children),
    {Owners1, Children1}.

start_children(_Addr, _Port, _Pass, 0, Accum) ->
    Accum;
start_children(Addr, Port, Pass, Count, Accum) ->
    case reddy_conn:connect(Addr, Port) of
        {ok, Pid} ->
            case do_auth(Pid, Pass) of
                ok ->
                    erlang:monitor(process, Pid),
                    start_children(Addr, Port, Pass, Count - 1, [Pid|Accum]);
                AuthError ->
                    AuthError
            end;
        Error ->
            Error
    end.

do_auth(Pid, undefined) ->
    Pid;
do_auth(Pid, Pass) ->
    reddy_server:auth(Pid, Pass).
