%% @doc Example of counter actor using first principals.
-module(counter_principal).

-export([start_link/0, start_link/1]).
-export([inc/1, dec/1, get/1, kill/1]).

% ----- Public -----
start_link() -> start_link(0).
start_link(N) -> erlang:spawn_link(fun()-> loop(N) end).

inc(Pid) -> Pid ! inc, ok.
dec(Pid) -> Pid ! dec, ok.
kill(Pid) -> Pid ! kill, ok.

get(Pid) ->
  Pid ! {get, self()},
  receive
    N when is_integer(N) -> N;
    _ -> error(interference)
  end.

% ----- Private -----
loop(N) ->
  NewN = receive
    inc -> N+1;
    dec -> N-1;
    kill -> exit(kill);
    {get, From} -> From ! N;
    _ -> N % noop for unhandled request
  end,
  loop(NewN).

% Usage
%   1> c(counter_principal).
%   {ok,counter_principal}
%   2> Pid = counter_principal:start_link().
%   <0.39.0>
%   3> counter_principal:inc(Pid).
%   ok
%   4> counter_principal:inc(Pid).
%   ok
%   5> counter_principal:get(Pid).
%   2
%   6> counter_principal:dec(Pid).
%   ok
%   7> counter_principal:get(Pid).
%   1
%   8> counter_principal:kill(Pid).
%   ** exception exit: killed
