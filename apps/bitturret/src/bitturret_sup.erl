-module(bitturret_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("bitturret_sup:init/1~n"),

    Children = [?CHILD(bitturret_handler_bridge, supervisor)],

    RestartStrategy = {one_for_one, 0, 1},

    {ok, {RestartStrategy, Children}}.