-module(listgen).
-export([gen_users/2, pos_int_only/1, get_rand_list/2]).


gen_users(Prefix, N) ->
    [Prefix ++ integer_to_list(X) || X <- lists:seq(1, N)].

pos_int_only(L) ->
    [X || X <- L, is_integer(X), X > 0].

get_rand_list(Count, MaxValue) ->
    rand:uniform(MaxValue),
    [rand:uniform(MaxValue) || _ <- lists:seq(1, Count)].