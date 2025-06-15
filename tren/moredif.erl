-module(moredif).
-export([start/0, keyval/0]).
-export([str/0, init/0, init_child/1]). % 13 task supervisor
-export([pmap/2]).
-export([proc/0, add/1, multiply/1, obrabotchik/0, stop/0]).


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
    register(?MODULE, Suppid),
    %Monitor = erlang:monitor(process, Suppid),
    {ok, Suppid}.

init() ->
    process_flag(trap_exit, true),
    lap(#super{}).

str_child(Name) ->
    Childpid = spawn_link(?MODULE, init_child, [Name]),
    Childpid.

init_child(Name) ->
    register(Name, self()),
    lap_child().


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
            case proplists:get_value(Name, State#super.child) of
                undefined ->
                    Childpid = str_child(Name),
                    From ! {ok, Childpid},
                    Newch = [{Name, Childpid} | State#super.child],
                    lap(State#super{child = Newch});
                Pid ->
                    io:format("Уже есть ~p~n", [Pid]),
                    lap(State)
            end;
        {From, stop_child, Name} ->
            case proplists:get_value(Name, State#super.child) of
                undefined ->
                    io:format("Такого нет ~n"),
                    From ! {error, not_found},
                    lap(State);
                Pid ->
                    Pid ! stop,
                    Newch = proplists:delete(Name, State#super.child),
                    lap(State#super{child = Newch})
            end;
        {'EXIT', Pid, Reason} ->
            case lists:keyfind(Pid, 2, State#super.child) of
                {Name, Pid} ->
                    io:format("перезапуск процесса ~p ~p~n", [Pid, Reason]),
                    Newch = proplists:delete(Name, State#super.child) ++ [{Name, str_child(Name)}],
                    lap(State#super{child = Newch});
                false ->
                    lap(State)
            end
        end.

%---------------------------------


child_pmap(From, Fun, N) ->
    Pid = spawn(fun() -> Result = Fun(N), From ! {self(), Result} end).

gath([], Acc) ->
    Acc;

gath([Pid | T], Acc) ->
    receive
        {Pid, Result} ->
            gath(T, Acc ++ [Result])
    end.

pmap(Fun, List) ->
    Listpid = [child_pmap(self(), Fun, N) || N <- List],
    gath(Listpid, []).


%---------------------------------


proc() ->
    Pid = spawn(fun() -> obrabotchik() end),
    register(?MODULE, Pid),
    {ok, Pid}.

addl([]) ->
    0;    
addl([H | T]) ->
    H + addl(T).

multiplyl([]) ->
    1;    
multiplyl([H | T]) ->
    H * multiplyl(T).

add(List) ->
    moredif ! {self(), add, List}.
multiply(List) ->
    moredif ! {self(), multiply, List}.
stop() ->
    moredif ! stop.

obrabotchik() ->
    receive
        {From, add, List} ->
            Res = addl(List),
            io:format("Result add: ~p~n", [Res]),
            From ! {ok, Res},
            obrabotchik();
        {From, multiply, List} ->
            Res = multiplyl(List),
            io:format("Result multiply: ~p~n", [Res]),
            From ! {ok, Res},
            obrabotchik();
        stop ->
            ok
    end.




