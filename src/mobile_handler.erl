-module(mobile_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3,
         websocket_init/3,
         websocket_terminate/3,
         websocket_handle/3,
         websocket_info/3]).

-record(user_info, {code,room_pid}).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, _Req, _Opts) ->
  {ok, _Req, #user_info{}}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

websocket_handle({text, Message}, Req, State) ->
  Event = event:parse(Message),
  handle_event(Event, Req, State).

websocket_info(joined_room, Req, State) ->
  Reply = event:joined_room(),
	{reply, {text, Reply}, Req, State};

websocket_info(open_estimation, Req, State) ->
  Event = event:open_estimation(),
  {RoomPid, Code} = user_info(State),
  RoomPid ! {user_ready_to_vote, Code},
	{reply, {text, Event}, Req, State};

websocket_info(finish_estimation, Req, State) ->
  Event = event:finish_estimation(),
	{reply, {text, Event}, Req, State};

websocket_info(cancel_estimation, Req, State) ->
  Event = event:cancel_estimation(),
	{reply, {text, Event}, Req, State}.

%-------------------------------------------------------------------------------
% Event Handling
%-------------------------------------------------------------------------------

handle_event(#{<<"event">> := <<"join_room">>}=Event, Req, State) ->
  Code = maps:get(<<"user_code">>, Event),
  RoomPid = doorman_server:use_room_reference(Code),
  RoomPid ! {user_connected, self(), Code},
  ReplyEvent = event:joined_room(),
  {reply, {text, ReplyEvent}, Req, State#user_info{code=Code,room_pid=RoomPid}};

handle_event(#{<<"event">> := <<"user_estimate">>}=Event, Req, State) ->
  Estimate = maps:get(<<"user_estimate">>, Event),
  {RoomPid, Code} = user_info(State),
  RoomPid ! {user_estimate, Code, Estimate},
  {ok, Req, State}.

%-------------------------------------------------------------------------------
% Internal Functions
%-------------------------------------------------------------------------------

user_info(State) ->
  RoomPid = State#user_info.room_pid,
  Code = State#user_info.code,
  {RoomPid, Code}.
