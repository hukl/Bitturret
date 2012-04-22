-module (bitturret_worker).
-export ([handle/1]).

% Connection id for testing purposes.
-define(CONN_ID, 890692333042557000).



% Pre-match on udp messages.
handle({udp, _Socket, _FromIP, _InPortNo, Packet}) ->
    _Response = handle_packet(Packet);
    % gen_udp:send(Socket, FromIP, InPortNo, Response);


% Handle errors bubbling up.
handle(Err = {error, _}) ->
    io:format("~p~n", [Err]);
    % FIXME - Proper error handling required!


% Ignore any other message.
handle(_Message) -> ignored.



% Handle announce requests.
handle_packet(
        <<
            ?CONN_ID:64/big,  % Make sure the client reads tracker response
            1:32/big,               % This is the action flag
            TransactionID:32/big,
            _InfoHash:160/bitstring, % The torrent Hash
            _PeerID:160/bitstring,
            _Downloaded:64/big,
            _Left:64/big,
            _Uploaded:64/big,
            _Event:32/big,
            _PeerIP:32/big,         % Ignored; use IP and Port from Socket
            _Key:32/big,
            _NumWant:32/big,
            _PeerPort:16/big,
            _Rest/binary
        >> ) ->

    Peers = [], % FIXME - Replace with call to judy-nif-cpp.
    PeersBin = << <<IP:32/big, Port:16/big>> || [IP,Port] <- Peers >>,

    <<
        1:32/big, TransactionID:32/big, 2160:32/big, 10:32/big, 5:32/big,
        PeersBin/binary
    >>;


% Handle connection requests.
handle_packet(
        <<
            _ConnectionID:64/big,
            0:32/big,
            TransactionID:32/big,
            _Rest/binary
        >> ) ->
    <<0:32/big, TransactionID:32/big, ?CONN_ID:64/big>>;


% Handle scrape requests.
handle_packet(
        <<
            ?CONN_ID:64/big,
            2:32/big,
            TransactionID:32/big,
            _Rest/binary
        >> ) ->
    <<2:32/big, TransactionID:32/big, 1:32/big, 3:32/big, 10:32/big>>;


% Discard all other packets.
handle_packet(_Packet) -> ignored.
