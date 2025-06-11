-module(list_rec).
-export([my_map/2, my_filter/2, fold/3]).

my_map(Fun, List) ->
    [Fun(I) || I <- List].

%---------------------------------

% my_fil_check(Fun, N) ->
%     TF = Fun(N),
%     if 
%         TF ->
%             N;
%         true ->
%             ok
            
%     end.

my_filter(Fun, List) ->
    [N || N <- List, Fun(N)].

%---------------------------------

fold(Fun, [], Acc) ->
    Acc;

fold(Fun, List, Acc) ->
    fold(Fun, tl(List), Fun(hd(List), Acc)).