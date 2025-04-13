-module(dup).

-export([dup/1, dup_tail/1, test/0]).


dup([]) -> [];
dup([Head | Tail]) -> [Head, Head | dup(Tail)].


dup_tail(L) ->
    lists:reverse(dup_tail_tail(L, [])).

dup_tail_tail([], Acc) ->
    Acc;

dup_tail_tail([Head | Tail], Acc) ->
    dup_tail_tail(Tail , [Head, Head | Acc]).

test() ->
    io:format("dup([1,2,3]): ~p~n", [dup([1,2,3])]),
    io:format("dup_tail([1,2,3]): ~p~n", [dup_tail([1,2,3])]).



