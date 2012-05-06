-module (bitturret_worker).
-export ([handle/1]).

% Connection id for testing purposes.
-define(CONN_ID,         16#0C5C5FF5332A6048).
-define(INITIAL_CONN_ID, 16#0000041727101980).



% Pre-match on udp messages.
handle({udp, Socket, FromIP, FromPort, Packet}) ->
    Response = handle_packet(FromIP, FromPort, Packet),

    gen_udp:send(Socket, FromIP, FromPort, Response);


% Handle errors bubbling up.
handle(Err = {error, _}) ->
    io:format("~p~n", [Err]);
    % FIXME - Proper error handling required!


% Ignore any other message.
handle(_Message) -> ignored.



% Handle announce requests.
handle_packet(FromIP, FromPort,
        <<
            ?CONN_ID:64/big,  % Make sure the client reads tracker response
            1:32/big,               % This is the action flag
            TransactionID:32/big,
            InfoHash:160/bitstring, % The torrent Hash
            _PeerID:160/bitstring,
            _Downloaded:64/big,
            _Left:64/big,
            _Uploaded:64/big,
            Event:32/big,
            _PeerIP:32/big,         % Ignored; use IP and Port from Socket
            _Key:32/big,
            _NumWant:32/big,
            _PeerPort:16/big,
            _Rest/binary
        >> ) ->

    case Event of
	0 ->
		io:format("none~n");
	1 ->
		io:format("completed~n");
	2 ->
		io:format("started~n");
	3 ->
		io:format("stopped~n");
        _ ->
		io:format("unknown event~n")
    end,

    {X,Y,_Z} = now(),
    io:format("got valid packet from ~p ~p  time:~p~n", [FromIP,FromPort,X*1000000+Y]),

    IntIP = ip_to_int(FromIP),

    addpeer({IntIP,FromPort},InfoHash),
    Peers = getpeers(InfoHash,10),

    io:format("got Peers: ~p~n", [Peers]),
    PeersBin = << <<IP:32/big, Port:16/big>> || {IP,Port} <- Peers >>,

    <<
        1:32/big, TransactionID:32/big, 2160:32/big, 10:32/big, 5:32/big,
        PeersBin/binary
    >>;


% Handle connection requests.
handle_packet(FromIP, FromPort,
        <<
            ?INITIAL_CONN_ID:64/big,
            0:32/big,
            TransactionID:32/big,
            _Rest/binary
        >> ) ->
    {X,Y,_Z} = now(),
    io:format("got connection request from ~p ~p  time:~p~n", [FromIP,FromPort,X*1000000+Y]),
    <<0:32/big, TransactionID:32/big, ?CONN_ID:64/big>>;


% Handle scrape requests.
handle_packet(FromIP, FromPort,
        <<
            ?CONN_ID:64/big,
            2:32/big,
            TransactionID:32/big,
            _Rest/binary
        >> ) ->
    {X,Y,_Z} = now(),
    io:format("got scrape request from ~p ~p  time:~p~n", [FromIP,FromPort,X*1000000+Y]),
    <<2:32/big, TransactionID:32/big, 1:32/big, 3:32/big, 10:32/big>>;


% Discard all other packets.
handle_packet(FromIP, FromPort, _Data) ->
    {X,Y,_Z} = now(),
    io:format("got invalid packet from ~p ~p  time:~p~n", [FromIP,FromPort,X*1000000+Y]),
    ignored.

% adds a peer to the list of peers for the infohash
addpeer(Peer, InfoHash) ->
    PeersTmp = ets:lookup(hashlist,InfoHash),
    case PeersTmp of
        [] ->
            ets:insert(hashlist,{InfoHash,[Peer]});
	_ ->
            {_InfoHash,PeerList} = hd(PeersTmp),
            ets:insert(hashlist,{InfoHash,[Peer|PeerList]})
    end.


getpeers(InfoHash,_Count) ->
    PeersTmp = ets:lookup(hashlist,InfoHash),
    {_InfoHash,PeerList} = hd(PeersTmp),
    PeerList.


ip_to_int({A,B,C,D}) -> (A*16777216)+(B*65536)+(C*256)+(D).

