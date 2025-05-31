-module(iotdev_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, SupPid} = iotdev_sup:start_link(),
    {ok, SupPid}.

stop(_State) ->
    ok.