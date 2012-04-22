-module (bitturret_handler).

% Packet buffer length.
-define(BUFLEN, 256).

% OTP supervisor callbacks.
-behaviour (supervisor).
-export ([start_link/0, init/1]).

% Internal, spawnable API.
-export ([loop_buffer/2, loop_accept/2, handle/1]).



% OTP supervisor boilerplate.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


% OTP supervisor boilerplate.
init([]) ->
    PortNo   = bitturret_config:get(port),
    BindAddr = bitturret_config:get(bind_addr),

    % Open the socket itself.
    SocketOpts = [
        binary,
        {ip, BindAddr},
        {active, true},
        {recbuf, 512},
        {read_packets, 16}
    ],
    Socket = gen_udp:open(PortNo, SocketOpts),

    % Start the buffer process, spawn the acceptor itself.
    BufferPid   = spawn(?MODULE, loop_buffer, [0, []]),
    AcceptorPid = spawn(?MODULE, loop_accept, [Socket, BufferPid]),
    register(acceptor, AcceptorPid),

    % No child processes for the supervisor.
    ignore.



% Attempt to accept new packets as fast as possible.
loop_accept(Socket, BufferPid) ->
    receive
        % Handle reported errors.
        Error = {error, _} ->
            gen_udp:close(Socket),
            error_logger:error_message("~p", [Error]);

        Message ->
            % Buffer messages first, continue accepting.
            BufferPid ! Message,
            loop_accept(Socket, BufferPid)
    end.


% Flush message buffer in case full.
loop_buffer(?BUFLEN, Buffer) ->
    flush_buffer(Buffer),
    loop_buffer(0, []);


% Add new messages to a non-full buffer.
loop_buffer(BufferLength, Buffer) ->
    receive
        % Prepend for performance.
        Message -> loop_buffer(1 + BufferLength, [Message|Buffer])
    after
        100 ->
            % Flush on timeout.
            flush_buffer(Buffer),
            loop_buffer(0, [])
    end.


% Handle all messages in the buffer. Immediately returns.
flush_buffer(Buffer) ->
    spawn(?MODULE, handle, [Buffer]).


% For every received message, make a new worker process handle it.
handle(Messages) ->
    lists:foreach(fun dispatch_worker/1, Messages).


% Dispatch a message to a new worker process.
dispatch_worker(Message) ->
    spawn(bitturret_worker, handle, Message).
