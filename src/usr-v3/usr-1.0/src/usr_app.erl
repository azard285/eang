-module(usr_app).
-export([start/2, stop/1]).

-behaviour(application).

start(_Type, _StartArgs) ->
    usr_sup:start_link().
stop(_State) ->
    ok.
