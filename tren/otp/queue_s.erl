% GenServer: Реализуй сервер очереди (queue) через gen_server.

%--------------------------------- 21 task
% Что такое сервер очереди?
% Это процесс, который:

% Хранит элементы в порядке FIFO (First In, First Out)

% Поддерживает операции:

% push — добавить элемент в конец

% pop — взять элемент из начала

% peek — посмотреть первый элемент без удаления


-module(queue_s).
-behaviour(gen_server).


-export([start/0, init/1, pop/0, push/1, peek/0, handle_call/3, handle_cast/2, show/0]).



start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(Atom) ->
    gen_server:cast(?MODULE, {push, Atom}).

pop() ->
    gen_server:call(?MODULE, pop).

peek() ->
    gen_server:call(?MODULE, peek).

show() ->
    gen_server:call(?MODULE, show).

init([]) ->
    {ok, []}.

handle_call(pop, _From, [H|T]) ->
    {reply, H, T};
handle_call(pop, _From, []) ->
    {reply, empty, []};
handle_call(peek, _From, [H|T]) ->
    {reply, H, [H|T]};
handle_call(peek, _From, []) ->
    {reply, empty, []};
handle_call(show, _From, [H|T]) ->
    {reply, [H|T], [H|T]};
handle_call(show, _From, []) ->
    {reply, empty, []}.


handle_cast({push, Atom}, State) ->
    {noreply, State ++ [Atom]}.
    


