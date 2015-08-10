-module(web_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3,
         websocket_init/3,
         websocket_terminate/3,
         websocket_handle/3,
         websocket_info/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, []}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

websocket_handle({text, Message}, Req, State) ->
  Event = event:parse(Message),
  handle_event(Event, Req, State).

websocket_info({user_connected, UserPid, Code}, Req, State) ->
  Event = event:user_connected(Code),
  {reply, {text, Event}, Req, [UserPid|State]};

websocket_info({user_estimate, Code, Estimate}, Req, State) ->
  Event = event:user_estimate(Code, Estimate),
  {reply, {text, Event}, Req, State};

websocket_info({user_ready_to_vote, Code}, Req, State) ->
  Event = event:user_ready_to_vote(Code),
  {reply, {text, Event}, Req, State}.

%-------------------------------------------------------------------------------
% Event Handling
%-------------------------------------------------------------------------------

handle_event(#{<<"event">> := <<"room_create">>}=Event, Req, State) ->
  Name = maps:get(<<"room_name">>, Event),
  Reply = event:room_created(Name),
  {reply, {text, Reply}, Req, State};

handle_event(#{<<"event">> := <<"user_invite">>}=Event, Req, State) ->
  Name = maps:get(<<"user_name">>, Event),
  Email = maps:get(<<"user_email">>, Event),
  Avatar = create_avatar_url_from_email(Email),
  Code = doorman_server:create_room_reference(),
  Reply = event:user_invited(Name, Email, Avatar, Code),
  {reply, {text, Reply}, Req, State};

handle_event(#{<<"event">> := <<"open_estimation">>}, Req, State) ->
  broadcast(open_estimation, State),
  {ok, Req, State};

handle_event(#{<<"event">> := <<"finish_estimation">>}, Req, State) ->
  broadcast(finish_estimation, State),
  {ok, Req, State};

handle_event(#{<<"event">> := <<"cancel_estimation">>}, Req, State) ->
  broadcast(cancel_estimation, State),
  {ok, Req, State}.

%-------------------------------------------------------------------------------
% Internal Functions
%-------------------------------------------------------------------------------

broadcast(Notification, []) ->
  {ok, Notification};

broadcast(Notification, [UserPid|T]) ->
  UserPid ! Notification,
  broadcast(Notification, T).

create_avatar_url_from_email(Email) ->
  MD5 = erlang:md5(Email),
  Hash = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= MD5]),
  Url = "http://www.gravatar.com/avatar/" ++ Hash,
  list_to_binary(Url).
