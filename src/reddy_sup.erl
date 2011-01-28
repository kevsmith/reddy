-module(reddy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 120,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    Children = [{'reddy_conn_sup', {reddy_conn_sup, start_link, []},
                 Restart, Shutdown, Type, [reddy_conn_sup]},
                {'reddy_pool_sup', {reddy_pool_sup, start_link, []},
                 Restart, Shutdown, Type, [reddy_pool_sup]}],

    {ok, {SupFlags, Children}}.
