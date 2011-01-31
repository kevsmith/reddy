### What is reddy?
reddy is simply my attempt at writing a better redis client in Erlang. It tries to address shortcomings
I saw in other clients like:

* Too low-level: Yes, you can manually shove binaries over a socket and make it work but that's hardly
a satisfying solution. reddy strives to provide a low-friction, self-documenting API. redis' excellent
command [docs](http://redis.io/commands) should serve equally well as docs for reddy's API.

* No support for multiple connections: While reddy is packaged as a proper OTP application it doesn't
make any assumptions about how you'd like to use it. Start as many connections as you need.

* No asynchronous API: reddy combines redis' native pipelining with Erlang's actor concurrency to provide
solid support for executing redis operations asynchronously.


### Simple connection example:
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

### Async connection example:
    1> application:start(reddy)
    ok
    2> {ok, Conn} = reddy_conn:connect("127.0.0.1", 6379).
    {ok, <0.50.0>}
    3> {ok, ResultId} = reddy_lists:lpush_(Conn, "bar", "1", true).
    {ok, #Ref<0.0.100>}
    4> receive {ResultId, Result} -> Result end.
    1
_Note: the trailing underscore on the function name indicates it is an async operation._