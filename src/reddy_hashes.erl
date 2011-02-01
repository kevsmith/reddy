-module(reddy_hashes).

-include("reddy_ops.hrl").

-export([hdel/3,
         hdel_/4,
         hexists/3,
         hexists_/4,
         hget/3,
         hget_/4,
         hgetall/2,
         hgetall_/3,
         hincrby/4,
         hincrby_/5,
         hkeys/2,
         hkeys_/3,
         hlen/2,
         hlen_/3,
         hmget/3,
         hmget_/4,
         hmset/3,
         hmset_/4,
         hset/4,
         hset_/5,
         hsetnx/4,
         hsetnx_/5,
         hvals/2,
         hvals_/3]).

hdel(Conn, Key, Field) when is_pid(Conn) ->
  reddy_conn:sync(Conn, ?HDEL, [Key, Field]).

hdel_(Conn, Key, Field, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HDEL, [Key, Field], WantsReturn).

hexists(Conn, Key, Field) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HEXISTS, [Key, Field]).

hexists_(Conn, Key, Field, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HEXISTS, [Key, Field], WantsReturn).

hget(Conn, Key, Field) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HGET, [Key, Field]).

hget_(Conn, Key, Field, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HGET, [Key, Field], WantsReturn).

hgetall(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HGETALL, [Key]).

hgetall_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HGETALL, [Key], WantsReturn).

hincrby(Conn, Key, Field, Increment) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HINCRBY, [Key, Field, Increment]).

hincrby_(Conn, Key, Field, Increment, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HINCRBY, [Key, Field, Increment], WantsReturn).

hkeys(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HKEYS, [Key]).

hkeys_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HKEYS, [Key], WantsReturn).

hlen(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HLEN, [Key]).

hlen_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HLEN, [Key], WantsReturn).

hmget(Conn, Key, Fields) when is_list(Fields),
                              is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HMGET, [Key, Fields]).

hmget_(Conn, Key, Fields, WantsReturn) when is_list(Fields),
                                            is_pid(Conn) ->
    reddy_conn:async(Conn, ?HMGET, [Key, Fields], WantsReturn).

hmset(Conn, Key, FieldValuePairs) when is_list(FieldValuePairs),
                                       is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HMSET, [Key, convert_field_value_pairs(FieldValuePairs)]).

hmset_(Conn, Key, FieldValuePairs, WantsReturn) when is_list(FieldValuePairs),
                                                     is_pid(Conn) ->
    reddy_conn:async(Conn, ?HMSET, [Key, convert_field_value_pairs(FieldValuePairs)], WantsReturn).

hset(Conn, Key, Field, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HSET, [Key, Field, Value]).

hset_(Conn, Key, Field, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HSET, [Key, Field, Value], WantsReturn).

hsetnx(Conn, Key, Field, Value) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HSETNX, [Key, Field, Value]).

hsetnx_(Conn, Key, Field, Value, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HSETNX, [Key, Field, Value], WantsReturn).

hvals(Conn, Key) when is_pid(Conn) ->
    reddy_conn:sync(Conn, ?HVALS, [Key]).

hvals_(Conn, Key, WantsReturn) when is_pid(Conn) ->
    reddy_conn:async(Conn, ?HVALS, [Key], WantsReturn).

%% Internal functions
convert_field_value_pairs(FVPairs) ->
    convert_field_value_pairs(FVPairs, []).

convert_field_value_pairs([], Accum) ->
    lists:reverse(Accum);
convert_field_value_pairs([{Field, Value}|T], Accum) ->
    Field1 = if
                 is_atom(Field) ->
                     list_to_binary(atom_to_list(Field));
                 true ->
                     Field
             end,
    convert_field_value_pairs(T, [[Field1, Value]|Accum]);
convert_field_value_pairs([H|T], Accum) ->
    convert_field_value_pairs(T, [H|Accum]).
