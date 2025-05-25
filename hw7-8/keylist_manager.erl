-module(keylist_manager).
-author("Pavlenko").
-description("Модуль для хранения списка ключ-значение с комментариями").
-include("keylist.hrl").

-export([start/0, init/0, loop/1, start_child/1, stop_child/1, stop/0, get_names/0]).

-type params() :: #{name => atom(), restart => permanent | temporary}.


%% @doc Запускает сервер keylist_manager.
-spec start() -> {ok, pid(), reference()}.
start() ->
    ServerPid = spawn(?MODULE, init, []),
    register(?MODULE, ServerPid),
    MonitorRef = erlang:monitor(process, ServerPid),
    {ok, ServerPid, MonitorRef}.

%% @doc Инициализирует сервер.
-spec init() -> no_return().
init() ->
    process_flag(trap_exit, true),
    loop(#state{}).

%% @doc Основной цикл сервера.
-spec loop(#state{}) -> no_return().
loop(State) ->
    receive
        {From, start_child, #{name := Name, restart := Restart}} ->
            case proplists:get_value(Name, State#state.children) of
                undefined ->
                    {ok, Pid} = keylist:start_link(Name),
                    NewChildren = [{Name, Pid} | State#state.children],
                    Newparm = case Restart of
                        permanent -> [Pid | State#state.permanent];
                        temporary -> State#state.permanent
                    end,
                    From ! {ok, Pid},
                    io:format("Процесс начат: ~p, ~p~n", [Name, Pid]),
                    loop(State#state{children = NewChildren, permanent = Newparm});
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
                    NewChildren = proplists:delete(Name, State#state.children),
                    Newparm = proplists:delete(Pid, State#state.permanent),
                    From ! ok,
                    loop(State#state{children = NewChildren, permanent = Newparm})
            end;

        stop ->
            io:format("Сервер завершил свою работу~n"),
            [Name ! stop || {Name, _} <- State#state.children],
            ok;

        {From, get_state} ->
            From ! State#state.children,
            loop(State);

        {'EXIT', Pid, Reason} ->
            case lists:keyfind(Pid, 2, State#state.children) of
                {Name, Pid} ->
                    case lists:member(Pid, State#state.permanent) of
                        true -> 
                            io:format("Перезапуск процесса: ~p~n", [Pid]),
                            {ok, NewPid} = keylist:start_link(Name),
                            NewChildren = lists:keystore(Name, 1, State#state.children, {Name, NewPid}),
                            Newparm = lists:delete(Pid, State#state.permanent) ++ [NewPid],
                            loop(State#state{children = NewChildren, permanent = Newparm});
                        false ->
                            io:format("Завершение процесса: ~p, ~p~n", [Pid, Reason]),
                            NewChildren = lists:keydelete(Pid, 2, State#state.children), 
                            Newparm = lists:delete(Pid, State#state.permanent),
                            loop(State#state{children = NewChildren, permanent = Newparm})
                    end;
                false -> 
                    loop(State)
            end
    end.

%% @doc Клиентская функция для запуска дочернего процесса.
-spec start_child(params()) -> ok.
start_child(Params) ->
    keylist_manager ! {self(), start_child, Params},
    ok.

%% @doc Клиентская функция для остановки дочернего процесса.
-spec stop_child(atom()) -> ok.
stop_child(Params) ->
    keylist_manager ! {self(), stop_child, Params},
    ok.

%% @doc Клиентская функция для остановки сервера.
-spec stop() -> ok.
stop() ->
    keylist_manager ! stop,
    ok.

%% @doc Клиентская функция для получения списка имен дочерних процессов.
-spec get_names() -> ok.
get_names() ->
    keylist_manager ! {self(), get_state},
    ok.
