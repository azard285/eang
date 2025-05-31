%%%-------------------------------------------------------------------
%% @doc iotdev_app public API
%% @end
%%%-------------------------------------------------------------------

-module(iotdev_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    iotdev_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
