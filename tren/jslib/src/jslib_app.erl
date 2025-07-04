%%%-------------------------------------------------------------------
%% @doc jslib public API
%% @end
%%%-------------------------------------------------------------------

-module(jslib_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    jslib_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
