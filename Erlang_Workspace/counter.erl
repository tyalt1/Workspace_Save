-module(counter).
-compile(export_all).
% Example of interprocess communication

start() -> start(0).
start(N) -> spawn_link(?MODULE, loop, [N]).

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
  NewN = receive
    inc -> N+1;
    dec -> N-1;
    %so on...
    terminate -> exit(terminate);
    {get, From} -> From ! N;
    _ -> N % noop for unhandled request
  end,
  loop(NewN).
