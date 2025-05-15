-module(ipv4_parser).
-export([parse_ipv4_header/1]).

-record(ipv4_header, {
    version, 
    ihl, 
    tos, 
    total_len, 
    ident, 
    flags, 
    frag_offset, 
    ttl, 
    proto, 
    checksum, 
    src_addr, 
    dst_addr, 
    option
}).

parse_ipv4_header(<<Version:4, Ihl:4, Tos:8, Total_len:16, Ident:16, Flags:3, Frag_offset:13, Ttl:8, Proto:8, Checksum:16, 
                    Src_addr1:8, Src_addr2:8, Src_addr3:8, Src_addr4:8, 
                    Dst_addr1:8, Dst_addr2:8, Dst_addr3:8, Dst_addr4:8, Option/binary>>) ->
    if
        Version > 4 -> throw({error, unsupported_version, Version});
        true ->
            Header = #ipv4_header{
                version = Version, 
                ihl = Ihl,
                tos = Tos,
                total_len = Total_len,
                ident = Ident,
                flags = Flags,
                frag_offset = Frag_offset,
                ttl = Ttl,
                proto = get_proto(Proto),
                checksum = Checksum,
                src_addr = {Src_addr1, Src_addr2, Src_addr3, Src_addr4},
                dst_addr = {Dst_addr1, Dst_addr2, Dst_addr3, Dst_addr4},
                option = if
                            Ihl > 5 -> 
                                Optionsize = (Ihl - 5) * 32,
                                <<Options:Optionsize, _/binary>> = Option,
                                Options;
                            true -> undefined
                        end
            },
            print_ipv4_header(Header)
    end;    

parse_ipv4_header(_) ->
    throw({error, invalid_packet}).

get_proto(Proto) ->
    case Proto of
        1 -> icmp;
        2 -> igmp;
        6 -> tcp;
        17 -> udp;  
        41 -> encap;
        89 -> ospf;
        132 -> sctp;
        _ -> Proto  
    end.

print_ipv4_header(Header) ->
    io:format("#ipv4_header {~n"),
    io:format("    version = ~p,~n", [Header#ipv4_header.version]),
    io:format("    ihl = ~p,~n", [Header#ipv4_header.ihl]),
    io:format("    tos = ~p,~n", [Header#ipv4_header.tos]),
    io:format("    total_len = ~p,~n", [Header#ipv4_header.total_len]),
    io:format("    ident = ~p,~n", [Header#ipv4_header.ident]),
    io:format("    flags = ~p,~n", [Header#ipv4_header.flags]),
    io:format("    frag_offset = ~p,~n", [Header#ipv4_header.frag_offset]),
    io:format("    ttl = ~p,~n", [Header#ipv4_header.ttl]),
    io:format("    proto = ~p,~n", [Header#ipv4_header.proto]),
    io:format("    checksum = ~p,~n", [Header#ipv4_header.checksum]),
    io:format("    src_addr = ~p,~n", [Header#ipv4_header.src_addr]),
    io:format("    dst_addr = ~p,~n", [Header#ipv4_header.dst_addr]),
    io:format("    option = ~p~n", [Header#ipv4_header.option]),
    io:format("}~n").