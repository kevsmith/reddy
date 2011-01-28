-module(reddy_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new_conn/2]).

-define(SERVER, ?MODULE).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_conn(Addr, Port) ->
    supervisor:start_child(?SERVER, [Addr, Port]).

init([]) ->
    Child = {reddy_conn, {reddy_conn, start_link, []},
             temporary, brutal_kill, worker, [reddy_conn]},
    Strategy = {simple_one_for_one, 0, 1},
    {ok, {Strategy, [Child]}}.
