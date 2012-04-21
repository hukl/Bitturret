-module(bitturret_handler).
-behavior(gen_server).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% Public API
-export([]).

-record(state, { socket }).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, Port}   = application:get_env(port),
    {ok, Socket} = gen_udp:open(Port, [binary, {ip, {127,0,0,1}}, {active, once}]),
    initialize_workers(0),
    { ok, #state{ socket = Socket } }.

handle_call( _Msg, _From, State ) ->
    { reply, ok, State }.

handle_cast( stop, State ) ->
    { stop, normal, State };

handle_cast( _Msg, State ) ->
    { stop, normal, State }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

handle_info( {udp, Socket, IP = {_,_,Shard,_}, Port, Msg}, State ) ->
    erlang:list_to_atom(integer_to_list(Shard)) ! [{Socket, IP, Port}, Msg],
    loop(State#state.socket, 0),
    { noreply, State }.

terminate( _Reason, _State ) ->
    whatever.

%% ===================================================================
%% Public API
%% ===================================================================

loop(Socket, Count) ->
    case (Count rem 100000) of
        0 -> error_logger:info_msg("Count: ~p~n", [Count]);
        _ -> ignore
    end,

    {ok, {IP = {_,_,Shard,_}, Port, Msg}} = gen_udp:recv(Socket, 0),
    erlang:list_to_atom(integer_to_list(Shard)) ! [{Socket, IP, Port}, Msg],
    loop(Socket, Count + 1).

initialize_workers(256) -> ok;

initialize_workers(Count) ->
    Pid  = spawn( bitturret_worker, loop, [] ),
    Name = erlang:list_to_atom(integer_to_list(Count)),
    register(Name, Pid),
    initialize_workers( Count + 1 ).



