-module(keylist_manager).
-include("keylist.hrl").

-export([start/0, init/0, loop/1]).

start() ->
    ServerPid = spawn(?MODULE, init, []),
    register(?MODULE, ServerPid),
    MonitorRef = erlang:monitor(process, ServerPid),
    {ok, ServerPid, MonitorRef}.

init() ->
    process_flag(trap_exit, true),
    loop(#state{}).

loop(State) ->
    receive
        {From, start_child, Name} ->
            case proplists:get_value(Name, State#state.children) of
                undefined ->
                    {ok, Pid} = keylist:start_link(Name),
                    NewChildren = [{Name, Pid} | State#state.children],
                    From ! {ok, Pid},
                    io:format("Процесс начат: ~p, ~p~n", [Name, Pid]),
                    loop(State#state{children = NewChildren});
                Pid -> 
                    From ! {ok, Pid},
                    loop(State)
            end;

        {From, stop_child, Name} ->
            case proplists:get_value(Name, State#state.children) of
                undefined ->
                    From ! {error, not_found},
                    loop(State);
                Pid -> 
                    exit(Pid, normal),
                    NewChildren = proplist:delete(Name, State#state.children),
                    From ! ok,
                    loop(State#state{children = NewChildren})
            end;

        stop ->
            io:format("Сервер завершил свою работу"),
            [Name ! stop || {Name, _} <- State#state.children],
            io:format("Сервер дети свою работу"),
            ok;

        {From, get_state} ->
            From ! State#state.children,
            loop(State);

        {'EXIT', Pid, Reason} ->
            io:format("Завершение процесса: ~p, ~p~n", [Pid, Reason]),
            NewChildren = lists:keydelete(Pid, 2, State#state.children), % Вот честно не смог найти другой выход кроме как использовать функции lists :(
            loop(State#state{children = NewChildren})
    end.
