-module(agent).
-behaviour(gen_server).

-export(
  [ state/1
  , start_link/1

  % API
  , cast/2
  , update/2
  , get/2

  %Callbacks
  , init/1
  , terminate/2
  , code_change/3
  , handle_call/3
  , handle_cast/2
  , handle_info/2
  ]).

-type state() :: term().

-type async_return() :: {noreply, state()} |
                        {noreply, state(), Timeout::pos_integer()} |
                        {noreply, state(), hibernate} |
                        {stop, Reason::term(), state()}.

-type sync_return() :: {reply, Reply::term(), state()} |
                       {reply, Reply::term(), state(), Timeout::pos_integer()} |
                       {reply, Reply::term(), state(), hibernate} |
                       {stop, Reason::term(), Reply::term(), state()} |
                       async_return().

% ----- Public -----
-spec start() -> {ok, pid()}.
start(Fun) -> gen_server:start(?MODULE, Fun, []).

-spec start_link() -> {ok, pid()}.
start_link(Fun) -> gen_server:start_link(?MODULE, Fun, []).

-spec cast(pid(), Fun::term()) -> ok.
cast(Pid, Fun) -> gen_server:cast(Pid, {cast, Fun}).

-spec update(pid(), Fun::term()) -> ok | {error, Reason::term()}.
update(Pid, Fun) -> gen_server:call(Pid, {update, Fun}).

-spec get(pid(), Fun::term()) -> {ok, Result::term()} | {error, Reason::term()}.
get(Pid, Fun) -> gen_server:call(Pid, {get, Fun}).

% ----- Callbacks -----
-spec init(term()) ->
  {ok, state()} |
  {ok, state(), Timeout::pos_integer()} |
  {ok, state(), hibernate}.
init(InitFun) -> {ok, InitFun()}.

-spec terminate(normal|shutdown|{shutdown,term()}|term(), state()) -> term().
terminate(_Reason, _State) -> ok.

-spec code_change(term()|{down, term()}, state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec handle_call(term(), {pid(),term()}, state()) -> sync_return().
handle_call({update, Fun}, _From, State) ->
  try Fun(State) of
    NewState -> {reply, ok, NewState}
  catch
    _:Reason -> {reply, {error, Reason}, State}
  end;
handle_call({get, Fun}, _From, State) ->
  try Fun(State) of
    Result -> {reply, {ok, Result}, State}
  catch
    _:Reason -> {reply, {error, Reason}, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), state()) -> async_return().
handle_cast({cast, Fun}, State) ->
  try Fun(State) of
    NewState -> {noreply, NewState}
  catch
    _:_ -> {noreply, State}
  end;
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(timeout | term(), state()) -> async_return().
handle_info(_Info, State) -> {noreply, State}.
