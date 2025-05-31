-module(keylist).
-export([start_link/1, init/1, loop/0]).

start_link(Name) ->
    Pid = spawn_link(?MODULE, init, [Name]),
    {ok, Pid}.

init(Name) ->
    register(Name, self()),
    loop().

loop() ->
    receive
        stop ->
            ok;

        _ ->
            loop()
    end.