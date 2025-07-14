-module(sipstream).
-export([start/0, stop/0, invite_callback/4, stream_audio/0, stream_audio_loop/2]).

-include_lib("nksip/include/nksip.hrl").

-define(DEFAULT_SIP_SERVER, "host.docker.internal").  
-define(DEFAULT_SIP_USER, "1000").
-define(DEFAULT_SIP_PASS, "1234").
-define(DEFAULT_AUDIO_URL, "http://icecast.omroep.nl/radio1-bb-mp3").
-define(DEFAULT_CALL_DURATION, 3600).

start() ->
    
    application:load(lager),
    application:set_env(lager, handlers, [
        {lager_console_backend, info}
    ]),
    
    
    {ok, _} = application:ensure_all_started(lager),
    {ok, _} = application:ensure_all_started(nksip),
    
    
    SipUser = os:getenv("SIP_USER", ?DEFAULT_SIP_USER),
    AudioUrl = os:getenv("AUDIO_URL", ?DEFAULT_AUDIO_URL),
    CallDuration = list_to_integer(os:getenv("CALL_DURATION", 
        integer_to_list(?DEFAULT_CALL_DURATION))),
    
    io:format("Starting SIP Streamer~nUser: ~s~nAudio: ~s~nDuration: ~ps~n",
              [SipUser, AudioUrl, CallDuration]),
    
    
    SipConfig = #{
        sip_from => "\"Streamer\" <sip:streamer@host.docker.internal>",
        sip_local_host => "host.docker.internal",
        sip_listen => "sip:all:5060",
        sip_transports => [{udp, all, 5060}],
        plugins => [nksip_uac_auto_auth],
        callback => {?MODULE, invite_callback, []}
    },
    
    case nksip:start_link(streamer, SipConfig) of
        {ok, _} ->
            io:format("NkSIP started successfully~n"),
            start_streaming(SipUser, AudioUrl, CallDuration);
        Error ->
            io:format("Failed to start NkSIP: ~p~n", [Error]),
            stop()
    end.

start_streaming(SipUser, AudioUrl, Duration) ->
    TwinkleIP = "host.docker.internal",  
    Uri = "sip:" ++ SipUser ++ "@" ++ TwinkleIP,
    io:format("Calling Twinkle at: ~s~n", [Uri]),
    
    
    os:cmd("mkfifo /tmp/audio_pipe 2>/dev/null"),
    
    
    spawn(fun() -> 
        Cmd = "ffmpeg -loglevel warning -i '" ++ AudioUrl ++ 
              "' -f mulaw -ar 8000 -ac 1 -y /tmp/audio_pipe",
        Port = open_port({spawn, Cmd}, [binary, stderr_to_stdout, exit_status]),
        io:format("FFmpeg command: ~s~n", [Cmd]),
        monitor_ffmpeg_port(Port)
    end),
    
    
    SDP = nksip_sdp:new("streamer", [
        {audio, [
            {port, 4000},
            {proto, udp},
            {ip, "host.docker.internal"},  
            {fmt, ["0"]},
            {rtpmap, "0", "PCMU/8000"},
            {ptime, 20}
        ]}
    ]),
    
    
    case nksip_uac:invite(streamer, Uri, [
        {sdp, SDP},
        {headers, [
            {"Contact", "<sip:streamer@host.docker.internal:5060>"},
            {"Content-Type", "application/sdp"},
            {"User-Agent", "SIPStreamer/1.0"}
        ]},
        {call_timeout, 30000}
    ]) of
        {ok, Code, _} when Code >= 200, Code < 300 ->
            io:format("Call established (code ~p)~n", [Code]),
            timer:sleep(Duration * 1000),
            stop();
        {ok, Code, _} ->
            io:format("Call rejected with code ~p~n", [Code]),
            stop();
        Error ->
            io:format("Call failed: ~p~n", [Error]),
            stop()
    end.


monitor_ffmpeg_port(Port) ->
    receive
        {Port, {data, Data}} ->
            io:format("FFmpeg output: ~p~n", [Data]),
            monitor_ffmpeg_port(Port);
        {Port, {exit_status, Status}} ->
            io:format("FFmpeg exited with status: ~p~n", [Status]);
        _Other ->
            monitor_ffmpeg_port(Port)
    after 5000 ->
        io:format("FFmpeg monitor timeout~n")
    end.


invite_callback({dialog_update, active, _}, _Dialog, _Req, _Args) ->
    io:format("Call active, starting audio stream~n"),
    spawn(fun() -> stream_audio() end),
    {reply, ok};
invite_callback({dialog_update, stop, _}, _Dialog, _Req, _Args) ->
    io:format("Call ended~n"),
    {reply, ok};
invite_callback(_Event, _Dialog, _Req, _Args) ->
    {reply, ok}.


stream_audio() ->
    {ok, Socket} = gen_udp:open(4000, [binary, {active, false}]),
    {ok, Pipe} = file:open("/tmp/audio_pipe", [read, binary, raw]),
    io:format("Audio streaming started~n"),
    stream_audio_loop(Socket, Pipe).

stream_audio_loop(Socket, Pipe) ->
    case file:read(Pipe, 160) of
        {ok, Data} ->
            gen_udp:send(Socket, {127,0,0,1}, 5004, Data),
            stream_audio_loop(Socket, Pipe);
        eof ->
            file:close(Pipe),
            gen_udp:close(Socket),
            io:format("Audio streaming stopped~n")
    end.


stop() ->
    io:format("Stopping SIP Streamer...~n"),
    nksip:stop(streamer),
    application:stop(nksip),
    application:stop(lager),
    os:cmd("rm -f /tmp/audio_pipe"),
    init:stop().