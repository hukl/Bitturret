-module(bitturret_worker).

-include("bitturret.hrl").

-export([handle/2]).


% Matches connection requests by patternmatching on the 0
handle( From, <<ConnectionID:64/big, 0:32/big, TransactionID:32/big, _Rest/binary>>) ->
    % error_logger:info_msg("Connection Request"),
    % error_logger:info_msg("C: ~p A: ~p T: ~p~n", [ConnectionID, 0, TransactionID]),
    Response = <<0:32/big, TransactionID:32/big, ?CONNECTION_ID:64/big>>,
    send_response( From, Response );

% Matches announce requests by patternmatching on the 1
handle(From = {_, PeerIP, _},
        <<
            ?CONNECTION_ID:64/big, % Make sure the client reads tracker response
            1:32/big,              % This is the action flag
            TransactionID:32/big,
            InfoHash:160/bitstring,% The torrent Hash
            PeerID:160/bitstring,
            Downloaded:64/big,
            Left:64/big,
            Uploaded:64/big,
            Event:32/big,
            _PeerIP:32/big,         % Ignored; use IP and Port from Socket
            Key:32/big,
            NumWant:32/big,
            PeerPort:16/big,
            _Rest/binary >>         % Some clients sent some padding bits
        ) ->

    case Event of
        0 -> ok;
            % error_logger:info_msg("Announce Request: Regular");
        1 -> ok;
            % error_logger:info_msg("Announce Request: Completed");
        2 ->
            % error_logger:info_msg("Announce Request: Started"),
            ets:insert(peers, {InfoHash, ip_to_int(PeerIP), PeerPort, leecher});
        3 ->
            % error_logger:info_msg("Announce Request: Stopped"),
            ets:match_delete(peers, {InfoHash, ip_to_int(PeerIP), PeerPort, '_'})
    end,

    Peers    = ets:match(peers, {InfoHash,'$1','$2','_'} ),

    % error_logger:info_msg("Peers: ~p~n", [Peers]),

    PeersBin = << <<IP:32/big,Port:16/big>> || [IP,Port] <- Peers >>,
    Response = <<1:32/big, TransactionID:32/big, 2160:32/big, 10:32/big, 5:32/big, PeersBin/binary>>,
    send_response(From, Response);

% Matches scrape requests by patternmatching on the 2
handle(From, <<?CONNECTION_ID:64/big, 2:32/big, TransactionID:32/big, Rest/binary>>) ->
    % error_logger:info_msg("Scrape Request: ~n"),
    Resp = <<2:32/big, TransactionID:32/big, 1:32/big, 3:32/big, 10:32/big>>;

handle(From, Msg) ->
    % error_logger:info_msg("Something else: ~p~n", [Msg]).

send_response({Socket, IP, Port}, Response) ->
    gen_udp:send(Socket, IP, Port, Response).

ip_to_int({A,B,C,D}) -> (A*16777216)+(B*65536)+(C*256)+(D).
