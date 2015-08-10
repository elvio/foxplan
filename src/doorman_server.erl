-module(doorman_server).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([start_link/0,
         create_room_reference/0,
         use_room_reference/1]).

%-------------------------------------------------------------------------------
% API
%-------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

create_room_reference() ->
  gen_server:call(?MODULE, create_room_reference).

use_room_reference(Code) ->
  gen_server:call(?MODULE, {use_room_reference, Code}).

%-------------------------------------------------------------------------------
% gen_server callbacks
%-------------------------------------------------------------------------------

init(_Arg) ->
  {ok, #{}}.

handle_call(create_room_reference, {Pid, _Ref}, State) ->
  Code = create_code(),
  NewState = maps:put(Code, Pid, State),

  {reply, Code, NewState};

handle_call({use_room_reference, Code}, _From, State) ->
  RoomPid = maps:get(Code, State),
  NewState = maps:remove(Code, State),

  {reply, RoomPid, NewState}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.

%-------------------------------------------------------------------------------
% Internal functions
%-------------------------------------------------------------------------------

create_code() ->
  S = letter() ++ number() ++ letter() ++ number(),
  list_to_binary(S).

random_index(List) ->
  random:uniform(length(List)).

letter() ->
  lists:nth(random_index(letters()), letters()).

letters() ->
  ["X", "Z", "U", "P", "Y"].

number() ->
  lists:nth(random_index(numbers()), numbers()).

numbers() ->
  ["2", "3", "4", "5", "6", "7", "8", "9"].
