-module(bitturret_worker).

-export([handle/2]).


handle(From, <<ConnectionID:64/big, 0:32/big, TransactionID:32/big>>) ->
    error_logger:info_msg("Connection Request"),
    error_logger:info_msg("C: ~p A: ~p T: ~p~n", [ConnectionID, 0, TransactionID]),
    Response = <<0:32/big, TransactionID:32/big, ConnectionID:64/big>>,
    send_response( From, Response );

handle(From,
        <<
            ConnectionID:64/big,
            Action:32/big,
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
    error_logger:info_msg("Info Hash: ~p~n", [InfoHash]),

    Peers = << <<IP:32/big,Port:16/big>> || {IP,Port} <- [{3261057657, 51413}] >>,
    Response = <<1:32/big, TransactionID:32/big, 60:32/big, 10:32/big, 5:32/big, Peers/binary>>,
    send_response(From, Response);

handle(From, Msg) ->
    error_logger:info_msg("Something else: ~p~n", [Msg]).

send_response({Socket, IP, Port}, Response) ->
    gen_udp:send(Socket, IP, Port, Response).
