-module(iotdev).
-behaviour(gen_server).


-export([start_link/0, add_device/1, delete_device/1, update_device/2, find_device/1]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(device, {id, name, address, temperature, metrics = []}).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_device(#{id := Id, name := Name, address := Address, temperature := Temp, metrics := Metrics}) ->
    Device = #device{id = Id, name = Name, address = Address, temperature = Temp, metrics = Metrics},
    gen_server:call(?MODULE, {add_device, Id, Device}).

delete_device(Id) ->
    gen_server:call(?MODULE, {delete_device, Id}).

update_device(Id, Updates) ->
    gen_server:call(?MODULE, {update_device, Id, Updates}).

find_device(Id) ->
    gen_server:call(?MODULE, {find_device, Id}).



init([]) ->
    iotdev_db:init(),
    {ok, #{}}.

handle_call({add_device, Id, Device}, _From, State) ->
    Reply = iotdev_db:store(Id, Device),
    {reply, Reply, State};

handle_call({delete_device, Id}, _From, State) ->
    Reply = iotdev_db:delete(Id),
    {reply, Reply, State};

handle_call({update_device, Id, Updates}, _From, State) ->
    case iotdev_db:lookup(Id) of
        {ok, Device} ->
            UpdatedDevice = apply_updates(Device, Updates),
            Reply = iotdev_db:update(Id, UpdatedDevice),
            {reply, Reply, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({find_device, Id}, _From, State) ->
    Reply = iotdev_db:lookup(Id),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(iotdev_dets),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


apply_updates(Device, Updates) ->
    lists:foldl(fun
        ({name, Value}, D) -> D#device{name = Value};
        ({address, Value}, D) -> D#device{address = Value};
        ({temperature, Value}, D) -> D#device{temperature = Value};
        ({metrics, Value}, D) -> D#device{metrics = Value}
    end, Device, maps:to_list(Updates)).