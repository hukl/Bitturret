-module (bitturret_config).
-export ([get/1, get/2, get/3]).



%% @doc Attempts to fetch the value for the given key,
%%      throws {error, {bitturret_config, Param}} otherwise.
get(Param) ->
  case ?MODULE:get(bitturret, Param, undefined) of
    undefined -> throw({error, {bitturret_config, Param}});
    Value -> Value
  end.


%% @doc Attempts to fetch the value for the given key,
%%      returns the Default otherwise.
get(Param, Default) ->
  ?MODULE:get(bitturret, Param, Default).


%% @doc Attempts to fetch the value for the given key from the
%       application's environment, returns the Default otherwise.
get(App, Param, Default) ->
  case application:get_env(App, Param) of
    {ok, Value} -> Value;
    _ -> Default
  end.
