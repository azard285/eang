-module(list_rec).
-export([my_map/2, my_filter/2, fold/3, flat/1]).

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

fold(_Fun, [], Acc) ->
    Acc;

fold(Fun, List, Acc) ->
    fold(Fun, tl(List), Fun(hd(List), Acc)).

%---------------------------------

% flat([]) ->
%     [];

% flat([H | T]) ->
%     if 
%         is_list(H) ->
%             flat(H);
%         true ->
%             [H] ++ flat(T)
%         end.
% 


flat_l(H) ->
    if
        is_list(H) ->
            case H of
                [] -> [];
                [HE | TL] -> flat_l(HE) ++ flat_l(TL)
            end;
        true ->
            H
        end.

flat(List) ->
    [flat_l(N) || N <- List].
