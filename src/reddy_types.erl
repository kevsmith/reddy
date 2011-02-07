-module(reddy_types).

-export([convert_field_value_pairs/1]).

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
