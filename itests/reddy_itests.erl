-module(reddy_itests).

-define(DEFAULT_HOST, "127.0.0.1").
-define(DEFAULT_PORT, "6379").

-define(LIST_KEY, <<"list_test1">>).

-include_lib("eunit/include/eunit.hrl").

-export([run/0]).


run() ->
    insure_host_port(),
    application:start(reddy),
    eunit:test(?MODULE).

all_test_() ->
    [{module, reddy_itest_lists},
     {module, reddy_itest_keys}].

insure_host_port() ->
    Host = case os:getenv("REDIS_HOST") of
               false ->
                   os:putenv("REDIS_HOST", ?DEFAULT_HOST),
                   ?DEFAULT_HOST;
               H ->
                   H
           end,
    Port = case os:getenv("REDIS_PORT") of
               false ->
                   os:putenv("REDIS_PORT", ?DEFAULT_PORT),
                   ?DEFAULT_PORT;
               P ->
                   P
           end,
    ?debugFmt("~n~n~n~n~n----------~n Using redis server at ~s:~s for integration tests~n----------~n~n~n~n", [Host, Port]),
    timer:sleep(5000).
