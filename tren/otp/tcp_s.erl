% TCP-сервер: Напиши простой TCP-сервер, который принимает строки и возвращает их в верхнем регистре.
%--------------------------------- 23 task

-module(tcp_s).

-export([start/1, stop/0]).


start(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, once}, {reuseaddr, true}, {active, false}]),
    io:format("Сервер запущен на порту ~p~n", [Port]),

    ets:new(serv, [named_table, public]),
    ets:insert(serv, {value, LSock}),

    Pid = [spawn(fun() -> accept_loop(LSock) end)],
    ets:insert(serv, {pids, Pid}),
    {ok, LSock}.

accept_loop(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            io:format("Новое подключение~n"),

            Pid = spawn(fun() -> handle(Socket) end),
            gen_tcp:controlling_process(Socket, Pid),
            ets:insert(serv, {pids, [Pid] ++ ets:lookup(serv, pids)}),

            accept_loop(LSock);
        {error, Reason} ->
            io:format("Ошибка подключение: ~s~n", [Reason]),
            accept_loop(LSock)
    end.

stop() ->
    [{pids, Pids}] = ets:lookup(serv, pids),
    [exit(N, normal) || N <- Pids],
        
    [{socket, LSock}] = ets:lookup(serv, socket),
    gen_tcp:close(LSock),
    ets:delete(serv),
    ok.

handle(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            String = unicode:characters_to_list(Data),
            UpString = verh_reg(String),
            io:format("получил: ~s~nотправляю: ~s~n", [String, UpString]),
            
            gen_tcp:send(Socket, UpString),
            handle(Socket);

        {error, closed} ->
            io:format("Клиент закрыл соединение~n"),
            gen_tcp:close(Socket);

        {error, Reason} ->
            io:format("Ошибка: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.
        
check(N) ->
    if 
        (N < 97) or (N > 122) ->
            false;
        true -> 
            true
    end.

verh_reg(Text) ->
    [N - 32 || N <- Text, check(N)].