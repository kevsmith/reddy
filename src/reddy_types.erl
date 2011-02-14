%% Copyright (c) 2011 Kevin Smith <kevin@hypotheticalabs.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

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
