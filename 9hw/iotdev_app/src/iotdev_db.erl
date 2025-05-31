-module(iotdev_db).
-export([init/0, store/2, lookup/1, delete/1, update/2, load_from_dets/0]).

-spec init() -> ok.
init() ->
    ets:new(iotdev_ets, [named_table, public, {keypos, 1}]),
    load_from_dets().

-spec load_from_dets() -> ok.
load_from_dets() ->
    {ok, ConfigBin} = file:read_file("config/dets_config.json"),
    #{<<"dets_file_path">> := DetsPath} = jsx:decode(ConfigBin, [return_maps]),
    {ok, iotdev_dets} = dets:open_file(iotdev_dets, [{file, binary_to_list(DetsPath)}]),
    dets:to_ets(iotdev_dets, iotdev_ets),
    ok.

-spec store(term(), term()) -> ok.
store(Id, Device) ->
    ets:insert(iotdev_ets, {Id, Device}),
    dets:insert(iotdev_dets, {Id, Device}),
    ok.

-spec lookup(term()) -> {ok, term()} | {error, not_found}.
lookup(Id) ->
    case ets:lookup(iotdev_ets, Id) of
        [{Id, Device}] -> {ok, Device};
        [] -> {error, not_found}
    end.

-spec delete(term()) -> ok.
delete(Id) ->
    ets:delete(iotdev_ets, Id),
    dets:delete(iotdev_dets, Id),
    ok.

-spec update(term(), map()) -> ok | {error, not_found}.
update(Id, NewDevice) ->
    case lookup(Id) of
        {ok, _} ->
            store(Id, NewDevice),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.