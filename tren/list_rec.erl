-module(list_rec).
-export([my_map/2]).

my_map(Fun, List) ->
    [Fun(I) || I <- List].