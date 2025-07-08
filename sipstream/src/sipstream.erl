-module(nksip_streamer).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_stream/2, stop_stream/0]).

-include_lib("nksip/include/nksip.hrl").

-define(RTP_PORT, 49170).
-define(LOCAL_IP, {127,0,0,1}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{sip_id => undefined, stream_pid => undefined, rtp_pid => undefined}}.

start_stream(Url, TargetSip) ->
    gen_server:call(?MODULE, {start_stream, Url, TargetSip}).

stop_stream() ->
    gen_server:call(?MODULE, stop_stream).

handle_call({start_stream, Url, TargetSip}, _From, State) ->
    case State of
        #{stream_pid := Pid} when is_pid(Pid) ->
            {reply, {error, already_streaming}, State};
        _ ->
            {ok, SipId} = nksip:start("audio_streamer", #{
                sip_from => "sip:streamer@localhost",
                sip_local_host => "localhost",
                transports => [{udp, all, 5060}]
            }),
            {ok, RtpPid} = start_rtp_server(),
            Pid = spawn_link(fun() -> stream_audio(SipId, Url, TargetSip, RtpPid) end),
            {reply, ok, State#{sip_id => SipId, stream_pid => Pid, rtp_pid => RtpPid}}
    end;
handle_call(stop_stream, _From, State = #{sip_id := SipId, stream_pid := Pid, rtp_pid := RtpPid}) ->
    exit(Pid, normal),
    exit(RtpPid, normal),
    nksip:stop(SipId),
    {reply, ok, State#{sip_id => undefined, stream_pid => undefined, rtp_pid => undefined}};
handle_call(stop_stream, _From, State) ->
    {reply, {error, not_streaming}, State}.

start_rtp_server() ->
    spawn_link(fun() -> rtp_server_loop(open_udp_port(?RTP_PORT)) end).

open_udp_port(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),
    Socket.

rtp_server_loop(Socket) ->
    receive
        {udp, Socket, _IP, _Port, _Data} ->
            % Просто принимаем пакеты, ничего не делаем
            rtp_server_loop(Socket);
        stop ->
            gen_udp:close(Socket)
    end.

stream_audio(SipId, Url, TargetSip, RtpPid) ->
    % Инициируем SIP-сессию
    {ok, DialogId} = nksip_uac:invite(SipId, TargetSip, [
        {body, nksip_sdp:new("streamer", [
            {<<"audio">>, ?RTP_PORT, [{<<"RTP/AVP">>, 0}]}  % PCMU/8000
        ])}
    ]),
    
    % Запускаем ffmpeg для преобразования аудио в RTP поток
    Cmd = lists:flatten(io_lib:format(
        "ffmpeg -i ~s -acodec pcm_mulaw -ar 8000 -ac 1 -f rtp rtp://127.0.0.1:~d",
        [Url, ?RTP_PORT]
    )),
    
    Port = erlang:open_port({spawn, Cmd}, [stderr_to_stdout, in, exit_status]),
    
    monitor_ffmpeg(Port, DialogId, SipId).

monitor_ffmpeg(Port, DialogId, SipId) ->
    receive
        {Port, {data, Data}} ->
            io:format("FFmpeg output: ~s~n", [Data]),
            monitor_ffmpeg(Port, DialogId, SipId);
        {Port, {exit_status, Status}} ->
            io:format("FFmpeg exited with status ~p~n", [Status]),
            nksip_uac:bye(DialogId, []);
        {nksip, {dialog_update, DialogId, _}} ->
            monitor_ffmpeg(Port, DialogId, SipId);
        {nksip, {stop, DialogId, _}} ->
            erlang:port_close(Port),
            ok
    after 1000 ->
        monitor_ffmpeg(Port, DialogId, SipId)
    end.