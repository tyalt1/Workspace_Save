%% @doc Example of counter actor using gen_server interface.
-module(counter_server).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([inc/1, dec/1, get/1, kill/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

% ----- Public -----
start_link() -> start_link(0).
start_link(N) -> gen_server:start_link(?MODULE, [N], []).

inc(Pid) -> gen_server:cast(Pid, inc).
dec(Pid) -> gen_server:cast(Pid, dec).
kill(Pid) -> gen_server:cast(Pid, kill).
get(Pid) -> gen_server:call(Pid, get).

% ----- Callbacks -----
init([N]) ->
  {ok, N}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(get, _From, N) ->
  {reply, N, N};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(inc, N) ->
  {noreply, N+1};
handle_cast(dec, N) ->
  {noreply, N-1};
handle_cast(kill, N) ->
  {stop, kill, N};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

% Usage
%   1> c(counter_server).
%   {ok,counter_server}
%   2> {ok, Pid} = counter_server:start_link().
%   {ok,<0.39.0>}
%   3> counter_server:inc(Pid).
%   ok
%   4> counter_server:inc(Pid).
%   ok
%   5> counter_server:get(Pid).
%   2
%   6> counter_server:dec(Pid).
%   ok
%   7> counter_server:get(Pid).
%   1
%   8> counter_server:kill(Pid).
%
%   =ERROR REPORT==== 29-Sep-2016::15:12:32 ===
%   ** Generic server <0.39.0> terminating
%   ** Last message in was {'$gen_cast',kill}
%   ** When Server state == 1
%   ** Reason for termination ==
%   ** kill
%   ** exception exit: killed
