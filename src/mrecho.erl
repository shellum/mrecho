-module(mrecho).

-export([start/0]).

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  ok = application:start(mrecho),
  public_key:start(),
  inets:start(),
  ssl:start().
