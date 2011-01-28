-module(reddy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    reddy_sup:start_link().

stop(_State) ->
    ets:delete(reddy_pools),
    ok.
