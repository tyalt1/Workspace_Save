-module({{name}}_sup).
-behaviour(supervisor).

-export([start_link/0]). % Public API
-export([init/1]). % Callback

% ----- Public -----
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% ----- Callbacks -----
-spec init(term()) -> {ok, supervisor:sup_flags(), [supervisor:child_spec()]}.
init([]) ->
  %sup_flags = {strategy, restart_intensity, period}
  %child_spec = {id, {M,F,A}, restart, shutdown, worker | supervisor, [modules]}
  {ok, {{one_for_one, 1, 5}, []}}.
