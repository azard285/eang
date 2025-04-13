-module(converter).
-export([rec_to_rur/1]).
-export([testrec/0]).

-record(conv_info, {type, amount, commission}).

-define(RATE_USD_RUR, 84).
-define(RATE_PESO_RUR, 4.13).
-define(RATE_YENE_RUR, 0.59).
-define(RATE_EURO_RUR, 96.22).
-define(RATE_FUNT_RUR, 108.87).
-define(DEBUG, true).

rec_to_rur(#conv_info{type=usd, amount=Amount, commission=Commission}) when is_integer(Amount), Amount > 0  ->
    ConvAmount = Amount * ?RATE_USD_RUR,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };

rec_to_rur(#conv_info{type=peso, amount=Amount, commission=Commission}) when is_integer(Amount), Amount > 0  ->
    ConvAmount = Amount * ?RATE_PESO_RUR,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };

rec_to_rur(#conv_info{type=yene, amount=Amount, commission=Commission}) when is_integer(Amount), Amount > 0  ->
    ConvAmount = Amount * ?RATE_YENE_RUR,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };

rec_to_rur(#conv_info{type=euro, amount=Amount, commission=Commission}) when is_integer(Amount), Amount > 0  ->
    ConvAmount = Amount * ?RATE_EURO_RUR,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };

rec_to_rur(#conv_info{type=funt, amount=Amount, commission=Commission}) when is_integer(Amount), Amount > 0  ->
    ConvAmount = Amount * ?RATE_FUNT_RUR,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };

rec_to_rur(#conv_info{type=OtherType, amount=Amount, commission=Commission}) ->
    if
        ?DEBUG == true ->
            io:format("Error: unsupported currency type ~p~n", [OtherType]);
        true ->
            ok
    end,
    {error, badarg}.


map_to_rub(#{type := usd, amount := Amount, commission := Commission}) ->
    Result = Amount * ?RATE_USD_RUR,
    CommissionResult = ConvAmount * Commission,
    {ok, Result - CommissionResult };




testrec() ->
    io:format("converter:rec_to_rur(#conv_info{type = usd, amount = 100, commission = 0.01}): ~p ~n~n", [rec_to_rur(#conv_info{type = usd, amount = 100, commission = 0.01})]),
    io:format("converter:rec_to_rur(#conv_info{type = peso, amount = 12, commission = 0.01}): ~p ~n~n", [rec_to_rur(#conv_info{type = peso, amount = 12, commission = 0.02})]),
    io:format("converter:rec_to_rur(#conv_info{type = yene, amount = 30, commission = 0.01}): ~p ~n~n", [rec_to_rur(#conv_info{type = yene, amount = 30, commission = 0.02})]),
    io:format("converter:rec_to_rur(#conv_info{type = euro, amount = -15, commission = 0.01}): ~p ~n~n", [rec_to_rur(#conv_info{type = euro, amount = -15, commission = 0.02})]),
    io:format("converter:rec_to_rur(#conv_info{type = funt, amount = 1, commission = 0.01}): ~p ~n~n", [rec_to_rur(#conv_info{type = funt, amount = 1, commission = 0.05})]).