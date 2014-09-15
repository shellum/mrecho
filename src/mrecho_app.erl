-module(mrecho_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(C_ACCEPTORS, 100).

start(_Type, _Args) ->
  Routes    = routes(),
  Dispatch  = cowboy_router:compile(Routes),
  Port      = port(),
  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}]}],
	{ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
	mrecho_sup:start_link().

stop(_State) ->
	ok.

routes() ->
  [
   {'_', [{"/", mrecho_handler, []}]}
  ].

port() ->
  case os:getenv("PORT") of
    false ->
      {ok, Port} = application:get_env(http_port),
      Port;
    Other ->
      list_to_integer(Other)
  end.
