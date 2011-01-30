-module(reddy_itest_lists).

-define(TEST_KEY, <<"reddy_lists">>).

-include_lib("eunit/include/eunit.hrl").
-include("reddy_itests.hrl").

create_and_delete_test() ->
    {ok, C} = ?CONNECT(),
    %% Delete key first to insure clean slate
    reddy_keys:del(C, ?TEST_KEY),
    ?assertMatch(0, reddy_keys:exists(C, ?TEST_KEY)),
    ?assertMatch(1, reddy_lists:lpush(C, ?TEST_KEY, 1)),
    ?assertMatch(1, reddy_keys:exists(C, ?TEST_KEY)),
    ?assertMatch(1, reddy_keys:del(C, ?TEST_KEY)),
    ?assertMatch(0, reddy_keys:exists(C, ?TEST_KEY)),
    reddy_conn:close(C).

lpush_and_lpop_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, ?TEST_KEY),
    ?assertMatch(1, reddy_lists:lpush(C, ?TEST_KEY, 1)),
    ?assertMatch(2, reddy_lists:lpush(C, ?TEST_KEY, 2)),
    ?assertMatch(3, reddy_lists:lpush(C, ?TEST_KEY, 3)),
    ?assertMatch(<<"3">>, reddy_lists:lpop(C, ?TEST_KEY)),
    ?assertMatch(<<"2">>, reddy_lists:lpop(C, ?TEST_KEY)),
    ?assertMatch(<<"1">>, reddy_lists:lpop(C, ?TEST_KEY)),
    reddy_conn:close(C).

rpush_and_rpop_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, ?TEST_KEY),
    ?assertMatch(1, reddy_lists:rpush(C, ?TEST_KEY, 1)),
    ?assertMatch(2, reddy_lists:rpush(C, ?TEST_KEY, 2)),
    ?assertMatch(3, reddy_lists:rpush(C, ?TEST_KEY, 3)),
    ?assertMatch(<<"3">>, reddy_lists:rpop(C, ?TEST_KEY)),
    ?assertMatch(<<"2">>, reddy_lists:rpop(C, ?TEST_KEY)),
    ?assertMatch(<<"1">>, reddy_lists:rpop(C, ?TEST_KEY)),
    reddy_conn:close(C).

linsert_lrange_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, ?TEST_KEY),
    ?assertMatch(1, reddy_lists:rpush(C, ?TEST_KEY, <<"Hello">>)),
    ?assertMatch(2, reddy_lists:rpush(C, ?TEST_KEY, <<"World">>)),
    ?assertMatch(3, reddy_lists:linsert(C, ?TEST_KEY, "BEFORE", <<"World">>, <<"There">>)),
    ?assertMatch([<<"Hello">>,
                  <<"There">>,
                  <<"World">>], reddy_lists:lrange(C, ?TEST_KEY, 0, -1)),
    reddy_conn:close(C).

lrem_test() ->
    {ok, C} = ?CONNECT(),
    reddy_keys:del(C, ?TEST_KEY),
    reddy_lists:rpush(C, ?TEST_KEY, <<"a">>),
    reddy_lists:rpush(C, ?TEST_KEY, <<"a">>),
    reddy_lists:rpush(C, ?TEST_KEY, <<"b">>),
    ?assertMatch(2, reddy_lists:lrem(C, ?TEST_KEY, 0, <<"a">>)),
    ?assertMatch(1, reddy_lists:llen(C, ?TEST_KEY)),
    reddy_conn:close(C).
