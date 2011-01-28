Simple connection example:
    1> application:start(reddy)
    ok
    2> {ok, Conn} = reddy_conn:connect("127.0.0.1", 6379).
    {ok, <0.50.0>}
    3> reddy_lists:lpush(Conn, "foo", "1").
    1
    4> reddy_lists:lpush(Conn, "foo", "1").
    2
    5> reddy_lists:lpop(Conn, "foo").
    <<"2">>
    6>

Async connection example:
    1> application:start(reddy)
    ok
    2> {ok, Conn} = reddy_conn:connect("127.0.0.1", 6379).
    {ok, <0.50.0>}
    %% Last argument indicates if the caller wants the return value
    %% or not
    3> {ok, ResultId} = reddy_lists:lpush_a(Conn, "bar", "1", true).
    {ok, #Ref<0.0.100>}
    4> 10> receive Msg -> Msg end.
    {#Ref<0.0.0.100>,1}