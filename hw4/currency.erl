-module(currency).

-export([to_rur2/1]).
-export([to_rur3/1]).
-export([test2/0, test3/0]).

-define(RATE_USD_RUR, 84).
-define(RATE_PESO_RUR, 4.13).
-define(RATE_YENE_RUR, 0.59).
-define(RATE_EURO_RUR, 96.22).


to_rur2({Type, Amount} = Arg) ->
    Result =
        case Arg of
            {usd, Amount} when is_integer(Amount), Amount > 0 ->
                {ok, ?RATE_USD_RUR * Amount};
            {peso, Amount} when is_integer(Amount), Amount > 0 ->
                %io:format("Convert ~p to rub, amount ~p~n", [peso, Amount]),%
                {ok, ?RATE_PESO_RUR * Amount};
            {yene, Amount} when is_integer(Amount), Amount > 0 ->
                %io:format("Convert ~p to rub, amount ~p~n", [yene, Amount]),%
                {ok, ?RATE_YENE_RUR * Amount};
            {euro, Amount} when is_integer(Amount), Amount > 0 ->
                %io:format("Convert ~p to rub, amount ~p~n", [euro, Amount]),%
                {ok, ?RATE_EURO_RUR * Amount};
        Error ->
            io:format("Can’t convert to rub, error ~p~n", [Error]),
            {error, badarg}
        end,
    io:format("Converted ~p to rub, amount ~p, Result ~p~n",
        [Type, Amount, Result]),
    Result.

to_rur3(Arg) ->
    case Arg of
        {usd, Amount} when is_integer(Amount), Amount > 0 ->
            %io:format("Convert ~p to rub, amount ~p~n", [usd, Amount]),
            {ok, ?RATE_USD_RUR * Amount};
        {peso, Amount} when is_integer(Amount), Amount > 0 ->
            %io:format("Convert ~p to rub, amount ~p~n", [peso, Amount]),
            {ok, ?RATE_PESO_RUR * Amount};
        {yene, Amount} when is_integer(Amount), Amount > 0 ->
            %io:format("Convert ~p to rub, amount ~p~n", [yene, Amount]),
            {ok, ?RATE_YENE_RUR * Amount};
        {euro, Amount} when is_integer(Amount), Amount > 0 ->
            %io:format("Convert ~p to rub, amount ~p~n", [euro, Amount]),
            {ok, ?RATE_EURO_RUR * Amount};
    Error ->
        io:format("Can’t convert to rub, error ~p~n", [Error]),
        {error, badarg}
    end.

test2() ->
    io:format("currency:to_rur2({usd, 100}): ~p ~n~n", [to_rur2({usd, 100})]),
    io:format("currency:to_rur2({peso, 12}): ~p~n~n", [to_rur2({peso, 12})]),
    io:format("currency:to_rur2({yene, 30}): ~p~n~n", [to_rur2({yene, 30})]),
    io:format("currency:to_rur2({euro, -15}): ~p~n~n", [to_rur2({euro, -15})]).

test3() ->
    io:format("currency:to_rur3({usd, 100}): ~p~n~n", [to_rur3({usd, 100})]),
    io:format("currency:to_rur3({peso, 12}): ~p~n~n", [to_rur3({peso, 12})]),
    io:format("currency:to_rur3({yene, 30}): ~p~n~n", [to_rur3({yene, 30})]),
    io:format("currency:to_rur3({euro, -15}): ~p~n~n", [to_rur3({euro, -15})]).