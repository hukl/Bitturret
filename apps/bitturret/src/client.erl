-module (client).
-compile (export_all).

-define (MSG,
    <<12,92,95,245,51,42,96,72,0,0,0,1,131,67,180,190,154,53,107,2,128,232,204,
      243,211,136,180,87,0,160,158,170,70,77,130,249,45,84,82,50,53,49,48,45,99,
      99,102,111,110,56,52,54,106,107,103,108,0,0,0,0,0,0,0,0,0,0,0,0,6,26,128,
      0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,49,123,193,195,0,0,0,80,233,194>>).

start(LocalIp, LocalPort, RemoteIp, RemotePort, PacketsPerSec) ->
    {ok, Socket} = gen_udp:open(LocalPort, [
        binary,
        {ip, LocalIp},
        {active, false}
    ]),
    loop_send(Socket, RemoteIp, RemotePort, PacketsPerSec).


loop_send(Socket, RemoteIp, RemotePort, PacketsPerSec) ->
    {Dur, _} = timer:tc(fun() ->
        times(PacketsPerSec, fun() ->
            ok = gen_udp:send(Socket, RemoteIp, RemotePort, ?MSG)
        end)
    end),
    SleepDur = erlang:trunc(1000 - (Dur / 1000)),
    if
        SleepDur < 0 ->
            throw({error, slow_client});
        true ->
            timer:sleep(SleepDur),
            loop_send(Socket, RemoteIp, RemotePort, PacketsPerSec)
    end.


times(0, _Fun) -> ok;

times(Times, Fun) ->
    Fun(),
    times(Times - 1, Fun).
