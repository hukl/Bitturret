-module (buffer_once).
-compile (export_all).

-define (MAX_BUFLEN, 100).
-define (MSG,
    <<12,92,95,245,51,42,96,72,0,0,0,1,131,67,180,190,154,53,107,2,128,232,204,
      243,211,136,180,87,0,160,158,170,70,77,130,249,45,84,82,50,53,49,48,45,99,
      99,102,111,110,56,52,54,106,107,103,108,0,0,0,0,0,0,0,0,0,0,0,0,6,26,128,
      0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,49,123,193,195,0,0,0,80,233,194>>).


start(Port, Ip) ->
    start(Port, Ip, []).

start(Port, Ip, Opts0) ->
    StatsPid = spawn(?MODULE, collect_stats, [0]),
    register(stats, StatsPid),
    timer:apply_interval(1000, ?MODULE, print_stats, []),

    BufPid = spawn(?MODULE, buffer_loop, [0, []]),
    register(buffer, BufPid),

    Opts = Opts0 ++ [
        binary,
        {ip, Ip},
        {active, once},
        {read_packets, 16},
        {recbuf, 32 * 1024}
    ],
    {ok, Socket} = gen_udp:open(Port, Opts),
    loop(Socket, BufPid).


print_stats() -> stats ! flush.

collect_stats(Overall0) ->
    receive
        flush ->
            io:format("~p~n", [Overall0]),
            collect_stats(0);
        NumPackets ->
            collect_stats(Overall0 + NumPackets)
    end.


loop(Socket0, BufPid) ->
    receive
        Packet ->
            % Allow another packet.
            inet:setopts(Socket0, [{active, once}]),
            BufPid ! Packet,
            loop(Socket0, BufPid)
    end.


buffer_loop(?MAX_BUFLEN, Buffer) ->
    buffer_loop(0, flush(Buffer));

buffer_loop(Buflen, Buffer) ->
    receive
        Packet ->
            % Append the message to the current buffer, be it new or old.
            buffer_loop(Buflen + 1, [Packet|Buffer])
    after
        % Replace with sensible value!
        1000 ->
            % No packets received within Xms? Manually flush the buffer.
            buffer_loop(0, flush(Buffer))
    end.


flush(Buffer) ->
    spawn(fun() ->
        stats ! length(Buffer)
    end),
    _NewBuffer = [].



client(Port, LocalIp, Ip, PacketsPerSec) ->
    PacketsPerInterval = PacketsPerSec / 1000,
    {ok, Socket} = gen_udp:open(Port, [binary, {ip, LocalIp}, {active, false}]),
    client_loop(Ip, Socket, PacketsPerInterval, PacketsPerInterval).


client_loop(Ip, Socket, NumPackets, 0) ->
    timer:sleep(1),
    client_loop(Ip, Socket, NumPackets, NumPackets);

client_loop(Ip, Socket, NumPackets, PacketsLeft) ->
    gen_udp:send(Socket, Ip, 1259, ?MSG),
    client_loop(Ip, Socket, NumPackets, PacketsLeft - 1).
