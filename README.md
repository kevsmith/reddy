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


### Synchronous API
    1> application:start(reddy)
    ok
    2> {ok, Conn} = reddy_conn:connect("127.0.0.1", 6379).
    {ok, <0.50.0>}
    3> reddy_lists:lpush(Conn, <<"foo">>, <<"1">>).
    1
    4> reddy_lists:lpush(Conn, <<"foo">>, <<"2">>).
    2
    5> reddy_lists:lpop(Conn, <<"foo">>).
    <<"2">>

### Async API
    1> application:start(reddy)
    ok
    2> {ok, Conn} = reddy_conn:connect("127.0.0.1", 6379).
    {ok, <0.50.0>}
    3> {ok, ResultId} = reddy_lists:lpush_(Conn, <<"bar">>, <<"1">>, true).
    {ok, #Ref<0.0.100>}
    4> receive {ResultId, Result} -> Result end.
    1

### Async fire n' forget API
    1> application:start(reddy)
    ok
    2> {ok, Conn} = reddy_conn:connect("127.0.0.1", 6379).
    {ok, <0.50.0>}
    %% Last function arg indicates whether or not the return value
    %% should be sent to the caller. Response is always parsed to prevent
    %% memory consumption due to accumulated pipelined responses.
    3> ok = reddy_lists:lpush_(Conn, <<"bar">>, <<"1">>, false).
    ok
    4>
_Note: the trailing underscore on the function name indicates it is an async operation._

### Connection pools
    1> application:start(reddy)
    ok
    2> reddy_pool:new_pool(production, [{ip, "127.0.0.1"}, {port, 6379}, {count, 10}]).
    {ok, <0.51.0>}
    3> reddy_lists:lpush(production, <<"foo">>, 1).
    1
    4> {ok, ResultId} = reddy_lists:lpop_(production, <<"foo">>, true)
    {ok,#Ref<0.0.0.159>}
    5> receive {ResultId, Results} -> Results end.
    <<"1">>
    6>

### Set hashes as proplists
    1> application:start(reddy)
    ok
    2> {ok, Conn} = reddy_conn:connect("127.0.0.1", 6379).
    {ok, <0.50.0>}
    3> reddy_hashes:hmset(C, <<"foo">>, [{today, <<"Tuesday">>}, {redis_is, <<"awesome">>}]).
    ok
    4> reddy_hashes:hvals(C, <<"foo">>).
    [<<"Tuesday">>,<<"awesome">>]

### TODO
* Support for ordered sets, pub/sub, transactions, and server commands
* Connection pool support for keys
* Pool management & introspection API
* Keep-alive logic for pooled connections
* More tests (!!!)
