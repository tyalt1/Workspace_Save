-module(task).

-export(
  [ start/1
  , start_link/1

  %API
  , async/1
  , await/1
  ]).

% ----- Public -----
-spec start(Fun::term()) -> {ok, pid()}.
start(Fun) -> spawn(Fun).

-spec start_link(Fun::term()) -> {ok, pid()}.
start_link(Fun) -> spawn_link(Fun).

-spec async(Fun::term()) -> pid().
async(Fun) ->
  spawn_link(do_async(Fun)).

-spec await(pid()) -> term().
await(Pid) ->
  Pid ! {task, await, self()},
  receive
    {task, return, Result} -> Result
  end.

% ----- Private -----
do_async(Fun) ->
  fun() ->
    Result = Fun(),
    receive
      {task, await, From} -> From ! {task, return, Result}
    end
  end.
