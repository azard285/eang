-module(moredif).
-export([start/0]).


start() ->
    Pid1 = spawn(fun() -> loop() end),
    Pid2 = spawn(fun() -> loop() end),
    Pid1 ! {Pid2, ping}.

loop() ->
    receive
        {From, ping} ->
            io:format("pong from ~p to ~p~n", [From, self()]),
            timer:sleep(2000),
            io:format("ping from ~p to ~p~n", [self(), From]),
            From ! {self(), ping}
    after 5000 ->
        io:format("timeout~n")
    end.

%---------------------------------