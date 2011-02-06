-module(reddy_itest_lists).

-define(TEST_KEY1, <<"reddy_lists1">>).
-define(TEST_KEY2, <<"reddy_lists2">>).

-include_lib("eunit/include/eunit.hrl").
-include("reddy_itests.hrl").

create_and_delete_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    ?assertMatch(0, reddy_keys:exists(C, ?TEST_KEY1)),
    ?assertMatch(1, reddy_lists:lpush(C, ?TEST_KEY1, 1)),
    ?assertMatch(1, reddy_keys:exists(C, ?TEST_KEY1)),
    ?assertMatch(1, reddy_keys:del(C, [?TEST_KEY1])),
    ?assertMatch(0, reddy_keys:exists(C, ?TEST_KEY1)),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_conn:close(C).

lpush_and_lpop_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    ?assertMatch(1, reddy_lists:lpush(C, ?TEST_KEY1, 1)),
    ?assertMatch(2, reddy_lists:lpush(C, ?TEST_KEY1, 2)),
    ?assertMatch(3, reddy_lists:lpush(C, ?TEST_KEY1, 3)),
    ?assertMatch(<<"3">>, reddy_lists:lpop(C, ?TEST_KEY1)),
    ?assertMatch(<<"2">>, reddy_lists:lpop(C, ?TEST_KEY1)),
    ?assertMatch(<<"1">>, reddy_lists:lpop(C, ?TEST_KEY1)),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_conn:close(C).

rpush_and_rpop_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    ?assertMatch(1, reddy_lists:rpush(C, ?TEST_KEY1, 1)),
    ?assertMatch(2, reddy_lists:rpush(C, ?TEST_KEY1, 2)),
    ?assertMatch(3, reddy_lists:rpush(C, ?TEST_KEY1, 3)),
    ?assertMatch(<<"3">>, reddy_lists:rpop(C, ?TEST_KEY1)),
    ?assertMatch(<<"2">>, reddy_lists:rpop(C, ?TEST_KEY1)),
    ?assertMatch(<<"1">>, reddy_lists:rpop(C, ?TEST_KEY1)),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_conn:close(C).

linsert_lrange_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    ?assertMatch(1, reddy_lists:rpush(C, ?TEST_KEY1, <<"Hello">>)),
    ?assertMatch(2, reddy_lists:rpush(C, ?TEST_KEY1, <<"World">>)),
    ?assertMatch(3, reddy_lists:linsert(C, ?TEST_KEY1, "BEFORE", <<"World">>, <<"There">>)),
    ?assertMatch([<<"Hello">>,
                  <<"There">>,
                  <<"World">>], reddy_lists:lrange(C, ?TEST_KEY1, 0, -1)),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_conn:close(C).

lrem_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_lists:rpush(C, ?TEST_KEY1, <<"a">>),
    reddy_lists:rpush(C, ?TEST_KEY1, <<"a">>),
    reddy_lists:rpush(C, ?TEST_KEY1, <<"b">>),
    ?assertMatch(2, reddy_lists:lrem(C, ?TEST_KEY1, 0, <<"a">>)),
    ?assertMatch(1, reddy_lists:llen(C, ?TEST_KEY1)),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_conn:close(C).

rpoplpush_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_keys:del(C, [?TEST_KEY2]),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"World!">>),
    reddy_lists:lpush(C, ?TEST_KEY1, <<"Hello">>),
    ?assertMatch(<<"World!">>, reddy_lists:rpoplpush(C, ?TEST_KEY1, ?TEST_KEY2)),
    reddy_keys:del(C, [?TEST_KEY1]),
    reddy_keys:del(C, [?TEST_KEY2]),
    reddy_conn:close(C).

blpop_two_conn_test() ->
    {ok, C1} = ?CONNECT(),
    {ok, C2} = ?CONNECT(),
    reddy_keys:del(C1, [?TEST_KEY1]),
    Me = self(),
    spawn(fun() ->
                  Result = reddy_lists:blpop(C1, ?TEST_KEY1, 1),
                  Me ! Result end),
    reddy_lists:rpush(C2, ?TEST_KEY1, <<"foo">>),
    ?assertMatch([?TEST_KEY1, <<"foo">>], receive
                                              M -> M
                                          end),
    reddy_keys:del(C2, [?TEST_KEY1]),
    reddy_conn:close(C1),
    reddy_conn:close(C2).

blpop_one_conn_test() ->
    {ok, C1} = ?CONNECT(),
    reddy_keys:del(C1, [?TEST_KEY1]),
    Me = self(),
    spawn(fun() ->
                  Result = reddy_lists:blpop(C1, ?TEST_KEY1, 1),
                  Me ! Result end),
    reddy_lists:rpush(C1, ?TEST_KEY1, <<"foo">>),
    ?assertMatch([?TEST_KEY1, <<"foo">>], receive
                                              M -> M
                                          end),
    reddy_keys:del(C1, [?TEST_KEY1]),
    reddy_conn:close(C1).

pooled_lpush_test() ->
    ?POOL(test_pool, 2),
    reddy_keys:del(test_pool, [?TEST_KEY1]),
    ?assertMatch(1, reddy_lists:lpush(test_pool, ?TEST_KEY1, 1)),
    ?assertMatch(2, reddy_lists:lpush(test_pool, ?TEST_KEY1, 2)),
    ?assertMatch(3, reddy_lists:lpush(test_pool, ?TEST_KEY1, 3)),
    ?assertMatch(<<"3">>, reddy_lists:lpop(test_pool, ?TEST_KEY1)),
    ?assertMatch(<<"2">>, reddy_lists:lpop(test_pool, ?TEST_KEY1)),
    ?assertMatch(<<"1">>, reddy_lists:lpop(test_pool, ?TEST_KEY1)),
    reddy_keys:del(test_pool, [?TEST_KEY1]),
    reddy_pool:close(test_pool).
