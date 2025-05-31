-module(iotdev).
-behaviour(gen_server).

-export([start_link/0, add_device/1, delete_device/1, update_device/2, find_device/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(device, {id, name, address, temperature, metrics = []}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_device(map()) -> ok.
add_device(#{id := Id, name := Name, address := Address, temperature := Temp, metrics := Metrics}) ->
    Device = #device{id = Id, name = Name, address = Address, temperature = Temp, metrics = Metrics},
    gen_server:call(?MODULE, {add_device, Id, Device}).

-spec delete_device(term()) -> ok.
delete_device(Id) ->
    gen_server:call(?MODULE, {delete_device, Id}).

-spec update_device(term(), map()) -> ok | {error, not_found}.
update_device(Id, Updates) ->
    gen_server:call(?MODULE, {update_device, Id, Updates}).

-spec find_device(term()) -> {ok, term()} | {error, not_found}.
find_device(Id) ->
    gen_server:call(?MODULE, {find_device, Id}).

-spec init([]) -> {ok, map()}.
init([]) ->
    iotdev_db:init(),
    {ok, #{}}.

-spec handle_call({add_device, term(), term()}, {pid(), term()}, map()) -> {reply, ok, map()}.
handle_call({add_device, Id, Device}, _From, State) ->
    Reply = iotdev_db:store(Id, Device),
    {reply, Reply, State}.

-spec handle_call({delete_device, term()}, {pid(), term()}, map()) -> {reply, ok, map()}.
handle_call({delete_device, Id}, _From, State) ->
    Reply = iotdev_db:delete(Id),
    {reply, Reply, State}.

-spec handle_call({update_device, term(), map()}, {pid(), term()}, map()) -> {reply, ok | {error, not_found}, map()}.
handle_call({update_device, Id, Updates}, _From, State) ->
    case iotdev_db:lookup(Id) of
        {ok, Device} ->
            UpdatedDevice = apply_updates(Device, Updates),
            Reply = iotdev_db:update(Id, UpdatedDevice),
            {reply, Reply, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

-spec handle_call({find_device, term()}, {pid(), term()}, map()) -> {reply, {ok, term()} | {error, not_found}, map()}.
handle_call({find_device, Id}, _From, State) ->
    Reply = iotdev_db:lookup(Id),
    {reply, Reply, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    dets:close(iotdev_dets),
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

apply_updates(Device, Updates) ->
    lists:foldl(fun
        ({name, Value}, D) -> D#device{name = Value};
        ({address, Value}, D) -> D#device{address = Value};
        ({temperature, Value}, D) -> D#device{temperature = Value};
        ({metrics, Value}, D) -> D#device{metrics = Value}
    end, Device, maps:to_list(Updates)).