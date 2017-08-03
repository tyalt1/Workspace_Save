%% @doc Module defines futures in Erlang.
-module(future).

-export(
  [ async/1
  , async/2
  , async/3

  , promise/0
  , deliver/2

  , await/1
  , await/2

  , pmap/2
  , pmap/3
  ]).

% ---- Public -----
%% @doc Equivalent to `async(Fun, [])`.
-spec async(Fun::term()) -> rpc:key().
async(Fun) when is_function(Fun) ->
  async(Fun, []).

%% @doc Make a asynchronous to me evaluated later. Returns Key to be used later.
-spec async(Fun::term(), list()) -> rpc:key().
async(Fun, Args) when is_function(Fun) ->
  rpc:async_call(node(), erlang, apply, [Fun, Args]).

%% @doc Like `async/1,2`, but function is called with Module, Fun, Args convention.
-spec async(atom(), atom(), list()) -> rpc:key().
async(Module, Fun, Args) when is_atom(Module) andalso is_atom(Fun) ->
  rpc:async_call(node(), Module, Fun, Args).

promise() ->
  async(fun() -> receive {deliver, Value} -> Value end end).

deliver(Pid, Value) ->
  Pid ! {deliver, Value}.

%% @doc Equivalent to `await(Key, infinity)`.
-spec await(rpc:key()) -> {ok, term()} | {error, timeout}.
await(Key) ->
  await(Key, infinity).

%% @doc
%%  Wait for async function call. Requires Key returned by `async/1,2,3`.
%%  Use timeout of 0 to return immediately, or infinity to block until result if returned.
-spec await(rpc:key(), infinity | non_neg_integer()) -> {ok, term()} | {error, timeout}.
await(Key, Timeout) ->
  case rpc:nb_yeild(Key, Timeout) of
    {value, Val} -> {ok, Val};
    timeout -> {error, timeout}
  end.

%% @doc Parallel map. Spawns future for each element, so may be expensive.
pmap(Fun, List) ->
  Wait = fun(Key) ->
    {ok, Val} = await(Key),
    Val
  end,
  lists:map(Wait, [ async(Fun, [Elem]) || Elem <- List ]).

%% @doc Parallel map, but imposes a batch size.
pmap(Fun, List, Batch) when Batch > 0 ->
  pmap(Fun, List, Batch, []).

% ----- Private -----
pmap(_, [], _, Acc) ->
  Acc;
pmap(Fun, List, Limit, Acc) when Limit > 0 ->
  {Front, Back} = case length(List) < Limit of
    true -> {List, []};
    false -> lists:split(Limit, List)
  end,
  pmap(Fun, Back, Limit, Acc ++ pmap(Fun, Front)).
