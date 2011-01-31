-module(reddy_time).

-export([now_to_unixts/1,
         future_to_unixts/2]).

now_to_unixts({Mega, Secs, _}) ->
    Mega * 1000000 + Secs.

future_to_unixts({Mega, Secs, _}, Offset) ->
    (Mega * 1000000 + Secs) + Offset.
