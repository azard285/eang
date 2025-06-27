% Работа с JSON: Используй библиотеку jsx для парсинга и генерации JSON.

%--------------------------------- 25 task

-module(js).

-export([test/0]).


encode(Text) ->
    jsx:encode(Text).

decode(Jsx) when is_binary(Jsx) ->
    jsx:decode(Jsx, [return_maps, {labels, attempt_atom}]).

test() ->
    Base = #{jopa => smeshno, nos => "dishit", 12 => 13},
    Enc = encode(Base),
    io:format("это -> ~p~nвот в это -> ~s~n~n", [Base, Enc]),
    
    io:format("это -> ~s~nвот в это -> ~p~n", [Enc, decode(Enc)]).


