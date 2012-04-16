-module(bitturret_test).

-export([start/1]).

-define(MSG, <<12,92,95,245,51,42,96,72,0,0,0,1,131,67,180,190,154,53,107,2,128,232,204,243,
  211,136,180,87,0,160,158,170,70,77,130,249,45,84,82,50,53,49,48,45,99,99,102,
  111,110,56,52,54,106,107,103,108,0,0,0,0,0,0,0,0,0,0,0,0,6,26,128,0,0,0,0,0,
  0,0,0,0,0,0,0,2,0,0,0,0,49,123,193,195,0,0,0,80,233,194>>).

start(Port) ->
  {ok,Socket} = gen_udp:open(Port, [binary, {ip, {127,0,0,1}}, {active, false}]),
  gen_udp:send(Socket, {127,0,0,1}, 6969, ?MSG),

  loop(Socket, 0).

loop(Socket, 10000000) ->
  gen_udp:close(Socket);

loop(Socket, Count) ->
  case (Count rem 10000) of
      0 -> error_logger:info_msg("Count: ~p~n", [Count]);
      _ -> ignore
  end,

  gen_udp:send(Socket, {127,0,0,1}, 6969, ?MSG),
  loop(Socket, Count + 1).