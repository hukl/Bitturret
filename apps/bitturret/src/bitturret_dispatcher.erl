-module(bitturret_dispatcher).
-behavior(gen_server).

% Managment API
-export([start/0, start_link/0, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% Public API
-export([]).

-record(state, { socket }).

%% ===================================================================
%% Management API
%% ===================================================================

start() ->
    gen_server:start( {local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %initialize_workers(0),
    {ok, []}.

handle_call( _Msg, _From, State ) ->
    { reply, ok, State }.

handle_cast( stop, State ) ->
    { stop, normal, State };

handle_cast( _Msg, State ) ->
    { stop, normal, State }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

handle_info( Msg, State ) ->
    { noreply, State }.

terminate( _Reason, _State ) ->
    whatever.

%% ===================================================================
%% Public API
%% ===================================================================

initialize_workers(256) -> ok;

initialize_workers(Count) ->
    Pid  = spawn( bitturret_worker, loop, [] ),
    Name = erlang:list_to_atom(integer_to_list(Count)),
    register(Name, Pid),
    initialize_workers( Count + 1 ).

