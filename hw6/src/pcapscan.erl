%%% pcapscan: reads pcap-file
%%%   open PCAP-file and read header
%%%   for each packet in file:
%%%     spawn process for packet inspection

% PCAP header: 24B
%       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%    0 |                          Magic Number                         |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%    4 |          Major Version        |         Minor Version         |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%    8 |                           Reserved1                           |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%   12 |                           Reserved2                           |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%   16 |                            SnapLen                            |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%   20 | FCS |f|0 0 0 0 0 0 0 0 0 0 0 0|         LinkType              |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

% PCAP Packet header: 16B
%                          1                   2                   3
%      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%    0 |                      Timestamp (Seconds)                      |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%    4 |            Timestamp (Microseconds or nanoseconds)            |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%    8 |                    Captured Packet Length                     |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%   12 |                    Original Packet Length                     |
%      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%   16 /                                                               /
%      /                          Packet Data                          /
%      /                        variable length                        /
%      /                                                               /
%      +---------------------------------------------------------------+

-module(pcapscan).
-export([file/1]).
-export([packet_analyzer_loop/1]).
-export([packet_analyzer_proc/2]).

-define(PCAP_HEADER_SIZE, 24).
-define(PCAP_PACKET_HEADER_SIZE, 16).
-define(PCAP_MAGICNUM_LE, 16#D4C3B2A1). % time stamps in Packet Records in seconds (little-endian)
-define(PCAP_MAGICNUM_BE, 16#4D3CB2A1). % time stamps in Packet Records in nanoseconds (big-endian)

file(FileName) ->
  case file:open(FileName, [binary, raw, read]) of
    {ok, IoDev} ->
      case pcap_read_header(IoDev) of
        {ok, BinData} ->
          {ok, Header} = pcap_parse_header(BinData),
          pcap_print_header(FileName, Header),
          PacketAnalyzerPid = packet_analyzer_start(),
          case pcap_read_packets_loop(IoDev, Header, 0, PacketAnalyzerPid) of
            {ok, _} -> 
              PacketAnalyzerPid ! {stop, self()},
              receive
                {ok, Results, _FromPid} ->
                  io:format("Packets: ~p~n", [Results])
              after 10000 ->
                io:format("error: timeout")
              end,
              {ok, ""};
            {error, _} -> {error, "Error reading packets"}
          end;
        {error, _} -> {error, "Error reading PCAP header"}
      end;
    {error, eof} -> {ok, ""};
    {error, Reason} -> {error, Reason}
  end.

pcap_read_header(IoDev) ->
  case file:read(IoDev, ?PCAP_HEADER_SIZE) of
    {ok, BinData} -> {ok, BinData};
    eof -> file:close(IoDev), {error, eof};
    {error, Reason} -> {error, Reason}
  end.

pcap_read_packets_loop(IoDev, PCAPHeader, PacketNo, PacketAnalyzerPid) ->
  case file:read(IoDev, ?PCAP_PACKET_HEADER_SIZE) of
    {ok, PHBin} ->
      {ok, PH} = pcap_parse_packet_header(pcap_header_endianess(PCAPHeader), PHBin),
      {ok, PD} = pcap_read_packet_data(IoDev, pcap_packet_cap_len(PH)),
      CurPacketNo = PacketNo + 1,
      PacketAnalyzerPid ! {packet, {CurPacketNo, PD}, self()},
      pcap_read_packets_loop(IoDev, PCAPHeader, CurPacketNo, PacketAnalyzerPid),
      {ok, ""};
    eof -> file:close(IoDev), {ok, eof};
    {error, Reason} -> {error, Reason}
  end.

pcap_header_endianess({pcapheader, ?PCAP_MAGICNUM_LE, _, _, _, _}) -> little;
pcap_header_endianess({pcapheader, ?PCAP_MAGICNUM_BE, _, _, _, _}) -> big.

pcap_parse_header(<<?PCAP_MAGICNUM_LE:32, _/binary>> = Header) ->
  % little-endian
  case Header of
    <<MagicNum:32/big, _:(32 * 3), SnapLen:32/little, FCS:3/little, FBit:1, _:12, LinkType:16/little>> ->
        {ok, {pcapheader, MagicNum, SnapLen, FCS, FBit, LinkType}};
    _Else -> {err, badpcap}
  end;
pcap_parse_header(<<?PCAP_MAGICNUM_BE:32, _/binary>> = Header) ->
  % big-endian
  case Header of
    <<MagicNum:32/big, _:(32 * 3), SnapLen:32/big, FCS:3/big, FBit:1, _:12, LinkType:16/big>> ->
        {ok, {pcapheader, MagicNum, SnapLen, FCS, FBit, LinkType}};
    _Else -> {err, badpcap}
  end.

pcap_parse_packet_header(little, PacketHeader) ->
  % little-endian
  case PacketHeader of
    <<TimestampSec:32/little, TimestampSubSec:32/little, CapPacketLen:32/little, OrigPacketLen:32/little>> ->
        {ok, {pcappacketheader, TimestampSec, TimestampSubSec, CapPacketLen, OrigPacketLen}};
    _Else -> {err, badpcappacket}
  end;
pcap_parse_packet_header(big, PacketHeader) ->
  % big-endian
  case PacketHeader of
    <<TimestampSec:32/big, TimestampSubSec:32/big, CapPacketLen:32/big, OrigPacketLen:32/big>> ->
        {ok, {pcappacketheader, TimestampSec, TimestampSubSec, CapPacketLen, OrigPacketLen}};
    _Else -> {err, badpcappacket}
  end.

pcap_packet_cap_len({pcappacketheader, _, _, CapPacketLen, _}) ->
  CapPacketLen.

pcap_read_packet_data(IoDev, PacketLen) when PacketLen >= 0 ->
  case file:read(IoDev, PacketLen) of
    {ok, PD} -> {ok, PD};
    eof -> file:close(IoDev), {error, eof};
    {error, Reason} -> {error, Reason}
  end.

pcap_print_header(FileName, {pcapheader, MagicNum, SnapLen, FCS, FBit, LinkType}) ->
  io:format("~p: MagicNum ~p, SnapLen ~p, FCS ~p ~p, LinkType ~p~n",
    [FileName, int2hexstr(MagicNum), SnapLen, FCS, FBit, LinkType]).

int2hexstr(Int) ->
  integer_to_list(Int, 16).

packet_analyzer_start() ->
  spawn(?MODULE, packet_analyzer_loop, [[]]).

packet_analyzer_loop(Results) ->
  receive
    {packet, Packet, _FromPid} ->
      spawn(?MODULE, packet_analyzer_proc, [Packet, self()]),
      packet_analyzer_loop(Results);
    {result, Result, _FromPid} -> 
      packet_analyzer_loop([Result | Results]);
    {stop, FromPid} ->      
      io:format("DEBUG: MessageBox: ~p~n", [process_info(self(), message_queue_len)]),
      FromPid ! {ok, Results, self()},
      ok
  after 10000 ->
    io:format("error: timeout")
  end.

packet_analyzer_proc(Packet, AnalyzerPid) ->
  % Parse Ethernet 2 frame: https://en.wikipedia.org/wiki/Ethernet_frame
  {PacketNo, PacketData} = Packet,
  <<_DestMAC:(6*8)/big, _SourceMAC:(6*8)/big, TypeLen:(2*8)/big, Rest/binary>> = PacketData,
  ARPPacket = parse_arp_packet(eth_type(TypeLen), Rest),
  AnalyzerPid ! {result, {PacketNo, byte_size(PacketData), eth_type(TypeLen), ARPPacket}, self()}.

eth_type(TypeLen) when TypeLen >= 0, TypeLen =< 1500 -> TypeLen;
eth_type(16#0800) -> ipv4;
eth_type(16#0806) -> arp;
eth_type(16#86dd) -> ipv6;
eth_type(_) -> ipv6.

parse_arp_packet(arp, Payload) ->
  <<
  Htype:16,
  Ptype:16,
  Hlen:8,
  Plen:8,
  Oper:16,
  Sha:48,
  SPA:32,
  Tha:48,
  TPA:32
  >> = Payload,


  {arp, arp_oper(Oper), int_to_ip(SPA), int_to_ip(TPA)};

parse_arp_packet(_, _) ->
    {nonarp}.

arp_oper(1) -> request;
arp_oper(2) -> reply.

int_to_ip(IntValue) ->
    <<A, B, C, D>> = <<IntValue:32>>,
    ipv4_to_str(A, B, C, D).

ipv4_to_str(A, B, C, D) ->
    lists:flatten(io_lib:format("~B.~B.~B.~B", [A, B, C, D])).
