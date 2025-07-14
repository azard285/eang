%%%-------------------------------------------------------------------
%% @doc sipstream public API
%% @end
%%%-------------------------------------------------------------------

-module(sipstream_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sipstream_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
