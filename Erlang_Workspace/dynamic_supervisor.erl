%% @doc Supervisor wrapper to simplify use of simple_one_for_one strategy.
-module(dynamic_supervisor).
-behaviour(supervisor).

-export([start_link/1, start_link/2, start_child/2, stop_child/2]). % Public API
-export([init/1]). % Supervisor Callback

-spec start_link(supervisor:child_spec()) -> {ok, pid()}.
start_link(ChildSpec) ->
  supervisor:start_link(?MODULE, ChildSpec).
-spec start_link(supervisor:sup_name(), supervisor:child_spec()) -> {ok, pid()}.
start_link(SupName, ChildSpec) ->
  supervisor:start_link(SupName, ?MODULE, ChildSpec).

% ----- Callback -----

init(ChildSpec) -> {ok, {simple_one_for_one, 1, 5}, [ChildSpec]}.
