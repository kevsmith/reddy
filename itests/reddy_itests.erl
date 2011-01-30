-module(reddy_itests).

-define(LIST_KEY, <<"list_test1">>).

-include_lib("eunit/include/eunit.hrl").

-export([run/2]).


run(Host, Port) ->
    application:start(reddy),
    os:putenv("REDIS_HOST", Host),
    os:putenv("REDIS_PORT", integer_to_list(Port)),
    eunit:test(?MODULE).

all_test_() ->
    [{module, reddy_itest_lists}].
