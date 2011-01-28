-module(reddy_async_test).

-export([start/0,
         start/2]).

start() ->
    start("127.0.0.1", 6379).

start(Addr, Port) ->
    application:start(reddy),
    {ok, Conn} = reddy_conn:connect(Addr, Port),
    Values = [integer_to_list(X) || X <- lists:seq(1, 10000)],
    _EnqueueRefs = enqueue(Conn, "test", Values),
    _DequeueRefs = dequeue(Conn, "test", Values),
    dump_messages(),
    reddy_conn:close(Conn).

enqueue(Conn, Key, Values) ->
    [reddy_conn:async(Conn, "LPUSH", [Key, Value]) || Value <- Values].


dequeue(Conn, Key, Values) ->
    [reddy_conn:async(Conn, "LPOP", [Key]) || _Value <- Values].

dump_messages() ->
    receive
        Msg ->
            io:format("~p~n", [Msg]),
            dump_messages()
    after 0 ->
            ok
    end.
