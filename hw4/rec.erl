-module(rec).
-export([fac/1, fac_tail/1, test/0]).

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N-1).

fac_tail(N) ->
    fac_tail_tail(N, 1).

fac_tail_tail(0, Acc) -> Acc;
fac_tail_tail(N, Acc) when N > 0 ->
    fac_tail_tail(N-1, N * Acc).

test() ->
    io:format("fac_tail(10): ~p~n", [fac_tail(10)]),
    io:format("fac_tail(0): ~p~n", [fac_tail(0)]).