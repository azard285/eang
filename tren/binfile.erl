% Чтение файла: Напиши функцию read_file(Path), которая читает содержимое файла и возвращает его в виде строки.

% Подсчет слов: Сделай функцию count_words(Text), которая считает количество слов в строке.

% Шифр Цезаря: Реализуй функции encode(Text, Shift) и decode(Text, Shift) для шифрования/дешифрования строки.

% Работа с бинарниками: Напиши функцию, которая преобразует строку в бинарник и наоборот.

% Парсинг CSV: Сделай простой парсер CSV-файла (разделитель — запятая).

-module(binfile).

-export([read_file/1, count_words/1, en_decode/2, csv_pars/1]).

read_file(File) ->
    {ok, Text} = file:read_file(File),
    io:format("~s~n", [Text]).



%---------------------------------

count_words(File) ->
    {ok, Text} = file:read_file(File),

    io:format("~p~n", [length(string:split(Text, " ", all))]).


%---------------------------------

en_decode(Text, Shift) ->
    List = [ case Shift >= 0 of
                true -> 
                    if 
                        N + Shift >= 127 ->
                            32 + (Shift - (127 - N));
                        true ->
                            N + Shift
                    end;
                false ->
                     if 
                        N + Shift =< 31 ->
                            126 + (Shift + (N - 32));
                        true ->
                            N + Shift
                    end
                end
                || N <- Text],
    io:format("Text/list ~p~n", [List]).


%---------------------------------

csv_pars(File) ->
    {ok, Text} = file:read_file(File),

    io:format("~s~n~n~n ~p ~n", [Text, string:split(Text, ",", all)]).


