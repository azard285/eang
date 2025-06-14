-module(moredif).
-export([start/0, keyval/0]).


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

-record(key_value, {pers = [] :: list({Key :: atom(), Value :: atom()})}).

keyval() ->
    spawn(fun() -> loop(#key_value{}) end).

loop(State) ->
    receive
        {put, Key, Value} ->
            case proplists:get_value(Key, State#key_value.pers) of
                undefined ->
                    Newpers = [{Key, Value} | State#key_value.pers],
                    loop(State#key_value{pers = Newpers});
                _ ->
                    Newpers = proplists:delete(Key, State#key_value.pers) ++ [{Key, Value}],
                    loop(State#key_value{pers = Newpers})
            end;
        {get, Key} ->
            case proplists:get_value(Key, State#key_value.pers) of
                undefined ->
                    io:format("такого нету"),
                    loop(State);
                _ ->
                    io:format("~p~n", [proplists:get_value(Key, State#key_value.pers)]),
                    loop(State)
            end
    end.


%---------------------------------

-record(super, {child = [] :: list({Name :: atom(), Pid :: pid()})}).

str() ->
    Suppid = spawn(?MODULE, init, []),
    register(?MODULE, Suppidp),
    Monitor = erlang:monitor(process, Suppid),
    {ok, Suppid, Monitor}.

init() ->
    process_flag(trap_exit, true),
    lap(#state{}).

lap_child() ->
    receive
        stop ->
            ok;
        Msg ->
            io:format("Msg: ~p~n", [Msg])
    end.

lap(State) ->
    receive 
        {From, start_child, Name} ->
            case proplists:get_value(State#super.child) of
                undefined ->
                    Childpid = spawn_link(fun() -> lap_child() end),
                    Newch = [{Name, Childpid} | State#super.child],
                    lap(State#super{child = Newch});
                Pid ->
                    io:format("Уже есть"),
                    lap(State)
            end;
        {From, stop_child, Name} ->
            case proplists:get_value(State#super.child) of
                undefined ->
                    io:format("Такого нет"),
                    lap(State);
                Pid ->
                    Pid ! stop,
                    Newch = proplists:delete(Name, State#super.child),
                    lap(State#super{child = Newch})
            end;
        {'EXIT', Pid, Reason} ->
            case lists:keyfind(Pid, 2, State#super.child) of
                {Name, Pid} ->
                    io:format("перезапуск процесса ~p~n", [Pid]),
                    Newch = proplists:delete(Name, State#super.child),
                    Childpid = spawn_link(fun() -> lap_child() end),
                    Newch ++ [Name, Childpid],
                    lap(State#super{child = Newch});
                false ->
                    lap(State)
            end
        end.




