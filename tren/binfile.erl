% Чтение файла: Напиши функцию read_file(Path), которая читает содержимое файла и возвращает его в виде строки.

% Подсчет слов: Сделай функцию count_words(Text), которая считает количество слов в строке.

% Шифр Цезаря: Реализуй функции encode(Text, Shift) и decode(Text, Shift) для шифрования/дешифрования строки.

% Работа с бинарниками: Напиши функцию, которая преобразует строку в бинарник и наоборот.

% Парсинг CSV: Сделай простой парсер CSV-файла (разделитель — запятая).

-module(binfile).

-export([read_file/1, count_words/1]).

read_file(File) ->
    {ok, Text} = file:read_file(File),
    io:format("~s~n", [Text]).



%---------------------------------

count_words(File) ->
    {ok, Text} = file:read_file(File),

    io:format("~p~n", [length(string:split(Text, " ", all))]).


