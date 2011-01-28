-module(reddy_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new_pool/2]).

-define(SERVER, ?MODULE).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_pool(Name, Options) ->
    supervisor:start_child(?SERVER, [Name, Options]).

init([]) ->
    Child = {reddy_pool, {reddy_pool, start_link, []},
             temporary, brutal_kill, worker, [reddy_pool]},
    Strategy = {simple_one_for_one, 0, 1},
    {ok, {Strategy, [Child]}}.
