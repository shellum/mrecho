-module(mrecho_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(C_ACCEPTORS, 100).

start(_Type, _Args) ->
  Dispatch  = cowboy_router:compile([
		{'_', [{"/", mrecho_handler, []}]}
		]),
  Port = port(),
  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}]}],
	{ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
	mrecho_sup:start_link().

stop(_State) ->
	ok.

port() ->
    case os:getenv("PORT") of
        false -> 8080;
        Other -> list_to_integer(Other)
    end.
