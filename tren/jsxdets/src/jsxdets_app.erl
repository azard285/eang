%%%-------------------------------------------------------------------
%% @doc jsxdets public API
%% @end
%%%-------------------------------------------------------------------

-module(jsxdets_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    jsxdets_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
