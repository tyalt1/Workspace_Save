-module(lock).
-behaviour(gen_fsm).

% Example of generic finite state machine

-export([start_link/1, unlock/2, lock/2]). %public api

-export([init/1, terminate/3, code_change/4,
         handle_event/3, handle_sync_event/4, handle_info/3]). %callbacks
-export([unlocked/2, locked/2, unlocked/3, locked/3]). %state exports

-record(state, {key :: integer()}).
% -opaque state() :: #state{}.
% -type state_name() :: unlocked | locked.
% -type event() :: {unlock | lock, integer()}.

% ----------- Public API -----------
start_link(Key) ->
  gen_fsm:start_link(?MODULE, [Key], []).

unlock(Name, Key) ->
  gen_fsm:sync_send_event(Name, {unlock, Key}).

lock(Name, Key) ->
  gen_fsm:sync_send_event(Name, {lock, Key}).

% ----------- Callbacks -----------
init([Key]) ->
  {ok, unlocked, #state{key=Key}}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

% async states
unlocked(_Event, State) ->
  {next_state, unlocked, State}.

locked(_Event, State) ->
  {next_state, locked, State}.

% sync states
unlocked({lock, Key}, _From, #state{key=Key} = State) ->
  io:format("Now locked...~n"),
  {reply, ok, locked, State};
unlocked({lock, _}, _From, #state{} = State) ->
  {reply, {error, wrong_key}, unlocked, State};
unlocked({unlock, _}, _From, #state{} = State) ->
  io:format("Already unlocked...~n"),
  {reply, ok, unlocked, State};
unlocked(_Event, _From, State) ->
  {reply, {error, invalid_message}, unlocked, State}.

locked({unlock, Key}, _From, #state{key=Key} = State) ->
  io:format("Now unlocked...~n"),
  {reply, ok, unlocked, State};
locked({unlock, _}, _From, #state{} = State) ->
  {reply, {error, wrong_key}, locked, State};
locked({lock, _}, _From, #state{} = State) ->
  io:format("Already locked...~n"),
  {reply, ok, unlocked, State};
locked(_Event, _From, State) ->
  {reply, {error, invalid_message}, unlocked, State}.

% handles
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.
