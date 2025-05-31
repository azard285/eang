-module(iotdev_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(atom(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    {ok, SupPid} = iotdev_sup:start_link(),
    {ok, SupPid}.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.