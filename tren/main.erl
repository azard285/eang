-module(main).
-export([fact/1, fib/1]).

fact(1) ->
    1;

fact(N) ->
    N * fact(N-1).

fib(1) ->
    0;

fib(2) ->
    1;  

% fib(3) ->
%     1;   

fib(N) ->
    fib(N-1) + fib(N-2).



