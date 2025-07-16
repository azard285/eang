-module(sipstream).
-export([start/0, stop/0, invite_callback/4, stream_audio/1, stream_audio_loop/2]).

-include_lib("nksip/include/nksip.hrl").

-define(DEFAULT_SIP_SERVER, "192.168.186.227").
-define(DEFAULT_SIP_USER, "1000").
-define(DEFAULT_SIP_PASS, "1234").
-define(DEFAULT_AUDIO_URL, "http://icecast.omroep.nl/radio1-bb-mp3").
-define(DEFAULT_CALL_DURATION, 3600).

start() ->
    application:set_env(lager, handlers, []),
    application:set_env(lager, error_logger_redirect, false),

    case application:ensure_all_started(nksip) of
        {ok, _} ->
            io:format("NkSIP started~n");
        {error, Reason} ->
            io:format("Error starting NkSIP: ~p~n", [Reason]),
            stop(),
            {error, Reason}
    end,

    SipUser = os:getenv("SIP_USER", ?DEFAULT_SIP_USER),
    AudioUrl = os:getenv("AUDIO_URL", ?DEFAULT_AUDIO_URL),
    CallDuration = list_to_integer(os:getenv("CALL_DURATION", 
        integer_to_list(?DEFAULT_CALL_DURATION))),
    HostIP = os:getenv("HOST_IP", ?DEFAULT_SIP_SERVER),
    {ok, LocalIP} = inet:getaddr(HostIP, inet),
    io:format("Local IP resolved: ~p~n", [LocalIP]),

    io:format("Starting SIP Streamer~nUser: ~s~nAudio: ~s~nDuration: ~ps~nHost: ~s~n",
              [SipUser, AudioUrl, CallDuration, HostIP]),

    CmdResult = os:cmd("nc -zuv " ++ HostIP ++ " 5062 2>&1"),
    io:format("Checking port 5062 (UDP): ~s~n", [CmdResult]),

    RtpCheck = os:cmd("nc -zuv " ++ HostIP ++ " 8000 2>&1"),
    io:format("Checking port 8000 (RTP, UDP): ~s~n", [RtpCheck]),

    SipConfig = #{
        sip_from => "\"Streamer\" <sip:streamer@" ++ HostIP ++ ">",
        sip_local_host => HostIP,
        sip_listen => "sip:all:5061",
        sip_transports => [{udp, all, 5061}],
        plugins => [nksip_uac_auto_auth],
        callback => {?MODULE, invite_callback, []}
    },

    case nksip:start_link(streamer, SipConfig) of
        {ok, Pid} ->
            io:format("NkSIP successfully started, PID: ~p~n", [Pid]),
            case nksip_uac:register(streamer, "sip:streamer@" ++ HostIP, [
                {sip_pass, ?DEFAULT_SIP_PASS},
                {contact, "<sip:streamer@" ++ HostIP ++ ":5062>"},
                {expires, 3600}
            ]) of
                {ok, 200, _} ->
                    io:format("Streamer registered in Asterisk~n"),
                    start_streaming(SipUser, AudioUrl, CallDuration, HostIP);
                {ok, Code, Resp} ->
                    io:format("Error registering streamer, code ~p: ~p~n", [Code, Resp]),
                    stop(),
                    {error, {registration_failed, Code}};
                {error, ErrorReason} ->
                    io:format("Error registering streamer: ~p~n", [ErrorReason]),
                    stop(),
                    {error, {registration_failed, ErrorReason}}
            end;
        {error, ErrorReason} ->
            io:format("Error starting NkSIP: ~p~n", [ErrorReason]),
            stop(),
            {error, ErrorReason}
    end.

start_streaming(SipUser, AudioUrl, Duration, HostIP) ->
    Uri = "sip:" ++ SipUser ++ "@" ++ HostIP ++ ":5062",
    io:format("Calling to: ~s~n", [Uri]),

    case os:cmd("curl -I " ++ AudioUrl ++ " 2>&1") of
        CurlResult ->
            io:format("Checking URL ~s: ~s~n", [AudioUrl, CurlResult])
    end,

    FFMpegCmd = "ffmpeg -loglevel info -timeout 10000000 -re -i '" ++ AudioUrl ++
                "' -f mulaw -ar 8000 -ac 1 -c:a pcm_mulaw -bufsize 128k -",
    io:format("Starting FFmpeg with command: ~s~n", [FFMpegCmd]),
    Port = open_port({spawn, FFMpegCmd}, [binary, exit_status]),
    put(ffmpeg_port, Port),

    SDP = nksip_sdp:new("streamer", [
        {audio, [
            {port, 4000},
            {proto, udp},
            {ip, HostIP},
            {fmt, ["0"]},
            {rtpmap, "0", "PCMU/8000"},
            {ptime, 20}
        ]}
    ]),

    io:format("Sending INVITE to ~s~n", [Uri]),
    case nksip_uac:invite(streamer, Uri, [
        {body, SDP},
        {headers, [
            {"Contact", "<sip:streamer@" ++ HostIP ++ ":5062>"},
            {"Content-Type", "application/sdp"},
            {"User-Agent", "SIPStreamer/1.0"}
        ]},
        {sip_auth_pass, ?DEFAULT_SIP_PASS},
        {call_timeout, 60000}
    ]) of
        {ok, Code, Resp} when Code >= 200, Code < 300 ->
            io:format("Call established (code ~p): ~p~n", [Code, Resp]),
            timer:sleep(Duration * 1000),
            ok;
        {ok, Code, Resp} ->
            io:format("Call rejected with code ~p: ~p~n", [Code, Resp]),
            stop(),
            {error, {invite_failed, Code}};
        {error, ErrorReason} ->
            io:format("Call error: ~p~n", [ErrorReason]),
            stop(),
            {error, {invite_failed, ErrorReason}}
    end.

invite_callback({dialog_update, active, _}, _Dialog, _Req, _Args) ->
    io:format("Call active, starting audio stream~n"),
    case get(ffmpeg_port) of
        undefined ->
            io:format("FFmpeg port not available, audio stream not started~n");
        Port ->
            spawn(fun() -> stream_audio(Port) end)
    end,
    {reply, ok};
invite_callback({dialog_update, stop, _}, _Dialog, _Req, _Args) ->
    io:format("Call ended~n"),
    stop(),
    {reply, ok};
invite_callback(_Event, _Dialog, _Req, _Args) ->
    {reply, ok}.

stream_audio(Port) ->
    case gen_udp:open(4000, [binary, {active, false}]) of
        {ok, Socket} ->
            io:format("Audio stream started, UDP socket opened on port 4000~n"),
            stream_audio_loop(Socket, Port);
        {error, ErrorReason} ->
            io:format("Error opening UDP port 4000: ~p~n", [ErrorReason]),
            stop(),
            {error, {udp_open_failed, ErrorReason}}
    end.

stream_audio_loop(Socket, Port) ->
    receive
        {Port, {data, Data}} ->
            io:format("Received FFmpeg data: ~p bytes~n", [byte_size(Data)]),
            case gen_udp:send(Socket, "192.168.186.227", 8000, Data) of
                ok ->
                    io:format("Sent RTP packet: ~p bytes to 192.168.186.227:8000~n", [byte_size(Data)]);
                {error, Reason} ->
                    io:format("Error sending RTP packet: ~p~n", [Reason])
            end,
            stream_audio_loop(Socket, Port);
        {Port, {exit_status, Status}} ->
            io:format("FFmpeg process terminated with status: ~p~n", [Status]),
            gen_udp:close(Socket),
            stop(),
            ok;
        {Port, closed} ->
            io:format("FFmpeg port closed~n"),
            gen_udp:close(Socket),
            stop(),
            ok
    after 30000 ->
        io:format("Audio stream timeout~n"),
        gen_udp:close(Socket),
        stop(),
        {error, audio_timeout}
    end.

stop() ->
    io:format("Stopping SIP Streamer...~n"),
    case erlang:whereis(streamer) of
        undefined ->
            io:format("Streamer not running, skipping nksip:stop~n");
        Pid when is_pid(Pid) ->
            case nksip:stop(streamer) of
                ok -> io:format("NkSIP stopped~n");
                {error, Reason} -> io:format("Error stopping NkSIP: ~p~n", [Reason])
            end
    end,
    case get(ffmpeg_port) of
        undefined -> ok;
        Port -> port_close(Port)
    end,
    application:stop(nksip),
    ok.