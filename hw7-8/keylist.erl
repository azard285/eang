-module(keylist).
-author("Pavlenko").
-description("Модуль для хранения списка ключ-значение с комментариями").
-include("keylist.hrl").

-export([
    start_link/1,
    init/1,
    loop/1,
    add/4,
    is_member/2,
    take/2,
    find/2,
    delete/2
]).

-type key() :: any().
-type value() :: any().
-type comment() :: any().
-type element() :: {key(), value(), comment()}.

%% @doc Запускает процесс keylist с именем.
-spec start_link(atom()) -> {ok, pid()}.
start_link(Name) ->
    Pid = spawn_link(?MODULE, init, [Name]),
    {ok, Pid}.

%% @doc Инициализирует процесс keylist.
-spec init(atom()) -> no_return().
init(Name) ->
    register(Name, self()),
    loop([]).

%% @doc Клиентская функция для добавления элемента.
-spec add(atom(), key(), value(), comment()) -> ok.
add(Name, Key, Value, Comment) ->
    Name ! {self(), add, Key, Value, Comment},
    ok.

%% @doc Клиентская функция для проверки наличия ключа.
-spec is_member(atom(), key()) -> ok.
is_member(Name, Key) ->
    Name ! {self(), is_member, Key},
    ok.

%% @doc Клиентская функция для извлечения элемента.
-spec take(atom(), key()) -> ok.
take(Name, Key) ->
    Name ! {self(), take, Key},
    ok.

%% @doc Клиентская функция для поиска элемента.
-spec find(atom(), key()) -> ok.
find(Name, Key) ->
    Name ! {self(), find, Key},
    ok.

%% @doc Клиентская функция для удаления элемента.
-spec delete(atom(), key()) -> ok.
delete(Name, Key) ->
    Name ! {self(), delete, Key},
    ok.

%% @doc Основной цикл процесса keylist.
-spec loop(list(element())) -> no_return().
loop(List) ->
    receive
        stop ->
            ok;

        {From, add, Key, Value, Comment} ->
            From ! ok,
            loop([{Key, Value, Comment} | List]);

        {From, is_member, Key} ->
            From ! {ok, lists:keymember(Key, 1, List)},
            loop(List);

        {From, take, Key} ->
            case lists:keytake(Key, 1, List) of
                {value, Element, NewList} ->
                    From ! {ok, Element},
                    loop(NewList);
                false ->
                    From ! {ok, not_found},
                    loop(List)
            end;

        {From, find, Key} ->
            case lists:keyfind(Key, 1, List) of
                Element when is_tuple(Element) ->
                    From ! {ok, Element};
                false ->
                    From ! {ok, not_found}
            end,
            loop(List);

        {From, delete, Key} ->
            NewList = lists:keydelete(Key, 1, List),
            From ! ok,
            loop(NewList)
    end.