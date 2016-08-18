-module({{name}}_server).
-behaviour(gen_server).

-export([start_link/0]). %Public API
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]). %Callbacks

-record(state, {}).
-type state() :: #state{}.

% ----- Public -----
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% ----- Private -----

% ----- Callbacks -----
-spec init(term()) ->
  {ok, state()} |
  {ok, state(), Timeout::pos_integer()} |
  {ok, state(), hibernate}.
init([]) ->
  {ok, #state{}}.

-spec terminate(normal|shutdown|{shutdown,term()}|term(), state()) -> term().
terminate(_Reason, _State) ->
  ok.

-spec code_change(term()|{down, term()}, state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec handle_call(term(), {pid(),term()}, state()) ->
  {reply, Reply::term(), state()} |
  {reply, Reply::term(), state(), Timeout::pos_integer()} |
  {reply, Reply::term(), state(), hibernate} |
  {noreply, state()} |
  {noreply, state(), Timeout::pos_integer()} |
  {noreply, state(), hibernate} |
  {stop, Reason::term(), Reply::term(), state()} |
  {stop, Reason::term(), state()}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), state()) ->
  {noreply, state()} |
  {noreply, state(), Timeout::pos_integer()} |
  {noreply, state(), hibernate} |
  {stop, Reason::term(), state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(timeout | term(), state()) ->
  {noreply, state()} |
  {noreply, state(), Timeout::pos_integer()} |
  {noreply, state(), hibernate} |
  {stop, Reason::term(), state()}.
handle_info(_Info, State) ->
  {noreply, State}.
