-module(event).
-export([parse/1, build/1]).

% Event builders
-export([room_created/1]).
-export([user_invited/4]).
-export([user_connected/1]).
-export([joined_room/0]).
-export([user_ready_to_vote/1]).
-export([user_estimate/2]).
-export([open_estimation/0]).
-export([cancel_estimation/0]).
-export([finish_estimation/0]).

parse(Event) ->
  jsx:decode(Event, [return_maps]).

build(Map) ->
  jsx:encode(Map).

room_created(Name) ->
  Map = #{
    <<"event">> => <<"room_created">>,
    <<"room_name">> => Name
  },

  build(Map).

user_invited(Name, Email, Avatar, Code) ->
  Map = #{
    <<"event">> => <<"user_invited">>,
    <<"user_name">> => Name,
    <<"user_email">> => Email,
    <<"user_avatar">> => Avatar,
    <<"user_code">> => Code
  },

  build(Map).

user_connected(Code) ->
  Map = #{
    <<"event">> => <<"user_connected">>,
    <<"user_code">> => Code
  },

  build(Map).

joined_room() ->
  Map = #{
    <<"event">> => <<"joined_room">>
  },

  build(Map).

user_estimate(Code, Estimate) ->
  Map = #{
    <<"event">> => <<"user_estimate">>,
    <<"user_code">> => Code,
    <<"user_estimate">> => Estimate
  },

  build(Map).

user_ready_to_vote(Code) ->
  Map = #{
    <<"event">> => <<"user_ready_to_vote">>,
    <<"user_code">> => Code
  },

  build(Map).

open_estimation() ->
  Map = #{
    <<"event">> => <<"open_estimation">>
  },

  build(Map).


finish_estimation() ->
  Map = #{
    <<"event">> => <<"finish_estimation">>
  },

  build(Map).

cancel_estimation() ->
  Map = #{
    <<"event">> => <<"cancel_estimation">>
  },

  build(Map).
