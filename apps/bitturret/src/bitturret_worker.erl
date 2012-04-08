-module(bitturret_worker).

-include("bitturret.hrl").

-export([handle/2]).


% Matches connection requests by patternmatching on the 0
handle(From, <<ConnectionID:64/big, 0:32/big, TransactionID:32/big>>) ->
    error_logger:info_msg("Connection Request"),
    error_logger:info_msg("C: ~p A: ~p T: ~p~n", [ConnectionID, 0, TransactionID]),
    Response = <<0:32/big, TransactionID:32/big, ?CONNECTION_ID:64/big>>,
    send_response( From, Response );

% Matches announce requests by patternmatching on the 1
handle(From,
        <<
            ?CONNECTION_ID:64/big, % Make sure the client reads tracker response
            1:32/big,              % This is the action flag
            TransactionID:32/big,
            InfoHash:160/bitstring,
            PeerID:160/bitstring,
            Downloaded:64/big,
            Left:64/big,
            Uploaded:64/big,
            Event:32/big,
            IPaddress:32/big,
            Key:32/big,
            NumWait:32/big,
            Port:16/big
        >>) ->

    error_logger:info_msg("Announce Request"),
    % error_logger:info_msg("ConnectionID: ~p~n", [ConnectionID]),

    Peers = << <<IP:32/big,Port:16/big>> || {IP,Port} <- [{3261057657, 51413}] >>,
    Response = <<1:32/big, TransactionID:32/big, 600:32/big, 10:32/big, 5:32/big, Peers/binary>>,
    send_response(From, Response);

% Matches scrape requests by patternmatching on the 2
handle(From, <<?CONNECTION_ID:64/big, 2:32/big, TransactionID:32/big, Rest/binary>>) ->
    error_logger:info_msg("Scrape Request: ~p~n");

handle(From, Msg) ->
    error_logger:info_msg("Something else: ~p~n", [Msg]).

send_response({Socket, IP, Port}, Response) ->
    gen_udp:send(Socket, IP, Port, Response).
