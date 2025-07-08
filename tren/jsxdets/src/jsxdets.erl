-module(jsxdets).
-behaviour(gen_server).

-export([start/0,  store/2, delete/1, find/1, update/2]).
-export([init/1,handle_cast/2, handle_call/3, handle_info/2]).

start() ->
    Pid = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    ets:new(store, [named_table, public]),
    dets_to_ets(),
    {ok, Pid}.


dets_to_ets() ->
    {ok, ConfigBin} = file:read_file("config/dets_config.json"),
    #{<<"dets">> := DetsPath} = jsx:decode(ConfigBin, [return_maps]),
    {ok, store_dets} = dets:open_file(store_dets, [{file, binary_to_list(DetsPath)}]),
    dets:to_ets(store_dets, store),
    ok.

init([]) ->
    {ok, []}.



store(Key, Val) ->
    gen_server:call(?MODULE, {stor, Key, Val}).

delete(Key) ->
    gen_server:call(?MODULE, {del, Key}).

find(Key) ->
    gen_server:call(?MODULE, {find, Key}).

update(Key, Val) ->
    gen_server:call(?MODULE, {update, Key, Val}).



handle_call({find, Key}, _From, State) ->
    Reply = case ets:lookup(store, Key) of
                [{Key, Val}] ->
                    Val;
                [] ->
                    {error, not_found}
            end,
    {reply, Reply, State};

handle_call({stor, Key, Val}, _From, State) ->
    ets:insert(store, {Key, Val}),
    dets:insert(store_dets, {Key, Val}),
    {reply, ok, State};

handle_call({del, Key}, _From, State) ->
    ets:delete(store, Key),
    dets:delete(store_dets, Key),
    {reply, ok, State};

handle_call({update, Key, Val}, _From, State) ->
    case ets:lookup(store, Key) of
        [{Key, _}] ->
            ets:delete(store, Key),
            dets:delete(store_dets, Key),

            ets:insert(store, {Key, Val}),
            dets:insert(store_dets, {Key, Val});
        [] ->
            store(Key, Val)
    end,
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Msg, State) ->
    {noreply, State}.