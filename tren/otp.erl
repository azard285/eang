% GenServer: Реализуй сервер очереди (queue) через gen_server.

% ETS таблица: Создай ETS-таблицу для быстрого доступа к данным.

% TCP-сервер: Напиши простой TCP-сервер, который принимает строки и возвращает их в верхнем регистре.

% Hot Code Swap: Сделай пример, где ты обновляешь код работающего процесса без остановки.

% Работа с JSON: Используй библиотеку jsx для парсинга и генерации JSON.



-module(otp).
-behaviour(gen_server).


-export([start/0, init/0, pop/0, push/1, peek/0, handle_call/3, handle_cast/2]).

%--------------------------------- 21 task
% Что такое сервер очереди?
% Это процесс, который:

% Хранит элементы в порядке FIFO (First In, First Out)

% Поддерживает операции:

% push — добавить элемент в конец

% pop — взять элемент из начала

% peek — посмотреть первый элемент без удаления


start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(Atom) ->
    gen_server:cast(?MODULE, {push, Atom}).

pop() ->
    gen_server:call(?MODULE, pop).

peek() ->
    gen_server:call(?MODULE, peek).


init() ->
    {ok, []}.

handle_call(pop, _From, [H|T]) ->
    {reply, H, T};
handle_call(pop, _From, []) ->
    {reply, empty, []};
handle_call(peek, _From, [H|T]) ->
    {reply, H, [H|T]};
handle_call(peek, _From, []) ->
    {reply, empty, []}.


handle_cast({push, Atom}, State) ->
    {noreply, State ++ [Atom]}.
    


