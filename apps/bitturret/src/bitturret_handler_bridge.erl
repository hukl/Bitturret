-module (bitturret_handler_bridge).

% OTP supervisor callbacks.
-behaviour (supervisor_bridge).
-export ([start_link/0, terminate/2, init/1]).


start_link() ->
    supervisor_bridge:start_link(
        {local, ?MODULE}, bitturret_handler_bridge, []).


% OTP boilerplate.
init(_Args) ->
    HandlerPid = spawn_link(bitturret_handler, start, []),
    {ok, HandlerPid, HandlerPid}.


% OTP boilerplate.
terminate(_Reason, Socket) ->
    gen_udp:close(Socket).
