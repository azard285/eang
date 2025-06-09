-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock_3).

-export([start_link/2,stop/0]).
-export([button/1,set_lock_button/1,set_code/1]).
-export([init/1,callback_mode/0,terminate/3]).
-export([handle_event/4]).

start_link(Code, LockButton) ->
    gen_statem:start_link(
        {local,?NAME}, ?MODULE, {Code,LockButton}, []).
stop() ->
    gen_statem:stop(?NAME).

button(Button) ->
    gen_statem:cast(?NAME, {button,Button}).
set_lock_button(LockButton) ->
    gen_statem:call(?NAME, {set_lock_button,LockButton}).
set_code(Code) ->
    gen_statem:call(?NAME, {set_code, Code}).

init({Code,LockButton}) ->
    process_flag(trap_exit, true),
    Data = #{code => Code, length => length(Code), buttons => [], incor => 0},
    {ok, {locked,LockButton}, Data}.

callback_mode() ->
    [handle_event_function,state_enter].


%% State: locked
handle_event(enter, _OldState, {locked,_}, Data) ->
    do_lock(),
    {keep_state, Data#{buttons := []}};

handle_event(state_timeout, button, {locked,_}, Data) ->
    {keep_state, Data#{buttons := []}};

handle_event(
  cast, {button,Button}, {locked,LockButton},
  #{code := Code, length := Length, buttons := Buttons, incor := Incor} = Data) ->
    NewButtons =
        if
            length(Buttons) < Length ->
                Buttons;
            true ->
                tl(Buttons)
        end ++ [Button],
    if
        length(NewButtons) < Length ->
            {keep_state, Data#{buttons := NewButtons}};
        NewButtons =:= Code -> 
            {next_state, {open,LockButton}, Data#{incor := 0}, [{state_timeout,30_000,button}]};
	true -> 
            if
                Incor == 2 ->
                    {next_state, {suspended, LockButton}, Data#{incor := 0}};
                true ->
                    {keep_state, Data#{buttons := NewButtons, incor := Incor + 1},
                     [{state_timeout,30_000,button}]} 
            end
    end;


%% State: suspended
handle_event(enter, _OldState, {suspended,_}, _Data) ->
    io:format("Error: uncorrect pass 3 times~n", []),
    {keep_state_and_data, [{state_timeout, 10_000, lock}]};

handle_event(state_timeout, lock, {suspended,LockButton}, Data) ->
    {next_state, {locked,LockButton}, Data};

handle_event(cast, {button,_}, {suspended,_}, _Data) ->
    {keep_state_and_data,[postpone]};


%% State: open
handle_event(enter, _OldState, {open,_}, _Data) ->
    do_unlock(),
    {keep_state_and_data,
     [{state_timeout,10_000,lock}]}; % Time in milliseconds

handle_event(state_timeout, lock, {open,LockButton}, Data) ->
    {next_state, {locked,LockButton}, Data};

handle_event(cast, {button,LockButton}, {open,LockButton}, Data) ->
    {next_state, {locked,LockButton}, Data};

handle_event(cast, {button,_}, {open,_}, _Data) ->
    {keep_state_and_data,[postpone]};



%% Common events
handle_event(
  {call,From}, {set_lock_button,NewLockButton},
  {StateName,OldLockButton}, Data) ->
    {next_state, {StateName,NewLockButton}, Data,
     [{reply,From,OldLockButton}]};

handle_event(
    {call, From}, {set_code, Code}, {open, LockButton}, Data) ->
        {keep_state, Data#{code := Code, length := length(Code), buttons := []},
        [{reply, From, ok}]};

handle_event(
  {call, From}, {set_code, _}, {StateName, _}, _Data) when StateName =/= open ->
    {keep_state_and_data, [{reply, From, {error, not_open}}]}.

    
do_lock() ->
    io:format("Locked~n", []).
do_unlock() ->
    io:format("Open~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
