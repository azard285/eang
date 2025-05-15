-module(exceptions).
-export([catch_all/1]).

catch_all(Action) when is_function(Action, 0) ->
    try Action() of
        Result -> {ok, Result}
    catch
        throw:Reason -> 
            io:format("Action ~p failed, reason ~p~n", [Action, Reason]),
            throw;

        error:Reason -> 
            io:format("Action ~p failed, reason ~p~n", [Action, Reason]),
            error;

        exit:Reason -> 
            io:format("Action ~p failed, reason ~p~n", [Action, Reason]),
            exit;

        _:_ ->
            io:format("This line will never be printed~n"), ok
    
    end.