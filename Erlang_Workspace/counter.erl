-module(counter).
-export([start/0, start/1, inc/1, dec/1, terminate/1, get/1]).

% Example of interprocess communication

start() -> start(0);
start(N) ->
  spawn_link(?MODULE, loop, [N]).

inc(Pid) -> Pid ! inc, ok.
dec(Pid) -> Pid ! dec, ok.
terminate(Pid) -> Pid ! terminate, ok.

get(Pid) ->
  Pid ! {get, self()},
  receive
    N when is_integer(N) -> N;
    _ -> error(interference)
  end.

loop(N) ->
  receive
    inc -> NewN = N+1;
    dec -> NewN = N-1;
    %so on...
    terminate -> exit(terminate);
    {get, From} -> From ! (NewN = N)
    _ -> undefined
  end,
  loop(NewN).
