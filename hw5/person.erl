-module(person).
-export([filter/2, all/2, any/2, update/2, get_average_age/1]).
-include("person.hrl").


filter(Fun, Person) -> lists:filter(Fun, Person).

all(Fun, Person) -> lists:all(Fun, Person).

any(Fun, Person) -> lists:any(Fun, Person).

update(Fun, Person) -> lists:map(Fun, Person).

get_average_age(Person) -> 
    {AgeSum, PersonsCount} = lists:foldl(
        fun(#person{age = Age}, {Sum, Count}) -> {Sum + Age, Count + 1} end,
        {0,0}, Person),

    case PersonsCount of
        0 -> 0;
        _ -> AgeSum / PersonsCount
    end.
