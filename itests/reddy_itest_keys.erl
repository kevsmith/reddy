-module(reddy_itest_keys).

-define(TEST_KEY1, <<"reddy_keys1">>).
-define(TEST_KEY2, <<"reddy_keys2">>).

-include_lib("eunit/include/eunit.hrl").
-include("reddy_itests.hrl").

missing_key_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    ?assertMatch(0, reddy_keys:exists(C, ?TEST_KEY1)),
    reddy_conn:close(C).

del_key_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    ?assertMatch(1, reddy_keys:exists(C, ?TEST_KEY1)),
    ?assertMatch(1, reddy_keys:del(C, [?TEST_KEY1])),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_conn:close(C).

expire_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    ?assertMatch(1, reddy_keys:expire(C, ?TEST_KEY1, 1)),
    %% Give redis time to expire the key
    ?WAIT(2000),
    ?assertMatch(0, reddy_keys:exists(C, ?TEST_KEY1)),
    reddy_conn:close(C).

expireat_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    ?assertMatch(1, reddy_keys:expireat(C, ?TEST_KEY1, erlang:now())),
    %% Give redis time to expire the key
    ?WAIT(2000),
    ?assertMatch(0, reddy_keys:exists(C, ?TEST_KEY1)),
    reddy_conn:close(C).

expireat_future_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    Expiry = reddy_time:future_to_unixts(erlang:now(), 1),
    ?assertMatch(1, reddy_keys:expireat(C, ?TEST_KEY1, Expiry)),
    %% Give redis time to expire the key
    ?WAIT(2000),
    ?assertMatch(0, reddy_keys:exists(C, ?TEST_KEY1)),
    reddy_conn:close(C).

keys_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1, ?TEST_KEY2]),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    reddy_lists:lpush(C, ?TEST_KEY2, <<"two">>),
    ?assertMatch([?TEST_KEY1, ?TEST_KEY2], reddy_keys:keys(C, <<"reddy_*">>)),
    ?assertMatch([], reddy_keys:keys(C, <<"nokeys*">>)),
    reddy_keys:del(C, [?TEST_KEY1, ?TEST_KEY2]),
    reddy_conn:close(C).

ttl_persist_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    ?assertMatch(1, reddy_keys:expire(C, ?TEST_KEY1, 5)),
    ?assertMatch(5, reddy_keys:ttl(C, ?TEST_KEY1)),
    ?assertMatch(1, reddy_keys:persist(C, ?TEST_KEY1)),
    ?assertMatch(-1, reddy_keys:ttl(C, ?TEST_KEY1)),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_conn:close(C).

random_key_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1, ?TEST_KEY2]),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    reddy_lists:lpush(C, ?TEST_KEY2, <<"two">>),
    Key = reddy_keys:randomkey(C),
    ?assert(lists:member(Key, [?TEST_KEY1, ?TEST_KEY2])),
    reddy_keys:del(C, [?TEST_KEY1, ?TEST_KEY2]),
    reddy_conn:close(C).

rename_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1, ?TEST_KEY2]),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    ?assertMatch(ok, reddy_keys:rename(C, ?TEST_KEY1, ?TEST_KEY2)),
    ?assertMatch([], reddy_keys:keys(C, ?TEST_KEY1)),
    ?assertMatch([?TEST_KEY2], reddy_keys:keys(C, ?TEST_KEY2)),
    reddy_keys:del(C, [?TEST_KEY1, ?TEST_KEY2]),
    reddy_conn:close(C).

renamenx_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1, ?TEST_KEY2]),
    ?assertMatch({error, _}, reddy_keys:renamenx(C, ?TEST_KEY1, ?TEST_KEY2)),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    ?assertMatch(1, reddy_keys:renamenx(C, ?TEST_KEY1, ?TEST_KEY2)),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"one">>),
    ?assertMatch(0, reddy_keys:renamenx(C, ?TEST_KEY1, ?TEST_KEY2)),
    reddy_keys:del(C, [?TEST_KEY1, ?TEST_KEY2]),
    reddy_conn:close(C).
