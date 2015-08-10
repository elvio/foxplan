-module(foxplan).
-export([start/0, stop/0]).

start() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowlib),
  application:start(cowboy),
  application:start(foxplan).

stop() ->
  application:stop(crypto),
  application:stop(ranch),
  application:stop(cowlib),
  application:stop(cowboy),
  application:stop(foxplan).
