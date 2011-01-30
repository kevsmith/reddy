-module(reddy_time).

-export([now_to_unixts/1]).

now_to_unixts({Mega, Secs, _}) ->
    Mega*1000000 + Secs.
