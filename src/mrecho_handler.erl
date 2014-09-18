-module(mrecho_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  Content = lasso:get_content("priv/index.htm",#{"name"=>"bob","age"=>"21"}),
  {ok, Req2} = cowboy_req:reply(200, [], Content, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
