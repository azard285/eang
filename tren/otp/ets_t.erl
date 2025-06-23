% ETS таблица: Создай ETS-таблицу для быстрого доступа к данным.

%--------------------------------- 22 task

-module(ets_t).

-export([init/0, push/2, get/1, del/1]).

init() ->
    ets:new(tabl, [named_table, public]).

push(Key, Val) ->
    ets:insert(tabl, {Key, Val}),
    ok.

get(Key) ->
    case ets:lookup(tabl, Key) of
        [{_, Val}] -> {ok, Val};
        [] -> {error, not_found}
    end.

del(Key) ->
    ets:delete(tabl, Key),
    ok.


