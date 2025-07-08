-module(list_rec).
-export([my_map/2, my_filter/2, fold/3, flat/1, rem_dup/1]).

my_map(Fun, List) ->
    [Fun(I) || I <- List].

%---------------------------------

my_filter(Fun, List) ->
    [N || N <- List, Fun(N)].

%---------------------------------

fold(_Fun, [], Acc) ->
    Acc;

fold(Fun, List, Acc) ->
    fold(Fun, tl(List), Fun(hd(List), Acc)).

%---------------------------------

flat(List) ->
    flatt(List, []).

flatt([H | T], Acc) ->
    if 
        is_list(H) ->
            flatt(T, flatt(H, Acc));
        true ->
            flatt(T, Acc ++ [H])
    end;

flatt([], Acc) ->
    Acc.

%---------------------------------

rem_dup(List) ->
    rem_dup(List, []).

rem_dup([H|T], Acc) ->
    case lists:member(H, Acc) of
        false ->
            rem_dup(T, Acc ++ [H]);
        true ->
            rem_dup(T, Acc)
        end;

rem_dup([], Acc) ->
    Acc.
