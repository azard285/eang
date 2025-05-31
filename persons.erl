-module(persons).
-export([get_persons/0]).

get_persons() ->
    [
        {1, "Mike", 23, male},
        {2, "Kate", 17, female},
        {3, "John", 19, male},
        {4, "Ivan", 29, male}
    ].