-module(mrecho_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, _Body, Req2} = cowboy_req:body(Req),
EventList = getMetrics(_Body, [], []),
io:format("eventlist: ~p",[EventList]),
Event = [{<<"gauges">>, element(2,EventList)}],
EncodedEvent = jsx:encode(Event),
  io:format("EVENT: ~p",[EncodedEvent]),
  Z = send_metric(EncodedEvent),
  %Z2 = jsx:encode(Z),
  Resp = "{code:'"++integer_to_list(Z)++"'}",

  {ok, Req3} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Resp, Req2),
  {ok, Req3, State}.

send_metric(Events) ->
  Headers = [{"Authorization", "Basic " ++ base64:encode_to_string(<<"cameron.shellum@octanner.com:a68a20cadf48a00347d7d99bd52d12b102350d8839ccb0cd58feccbe732116c6">>)}],
  case httpc:request(post, {"https://metrics-api.librato.com/v1/metrics", Headers, "application/json", Events}, [], []) of
    {ok, {{_, 200, _}, _Header, _Body}} ->
      200;
    {ok, {{_, I, _}, _Header, _Body}} when is_integer(I) ->
      I;
    {error, socket_closed_remotely} ->
      {error, <<"No socket">>};
    _ -> 0
  end.

terminate(_Reason, _Req, _State) ->
  ok.


getMetrics(Str,Source,MeasureList) ->
  case Str of
    <<"measure#",_Rest/binary>> ->
      KVPair = extractMetric(Str),
      [K,V] = binary:split(list_to_binary(KVPair), [<<"=">>],[]),
      TunedV = binary:replace(V,<<"ms">>,<<"">>),
      NewStart = size(K)+1+size(V)+string:len("measure#"),
      getMetrics(list_to_binary(string:substr(binary_to_list(Str),NewStart)), Source, [[{name, K},{value,TunedV}] | MeasureList]);
    <<"source=", _Rest/binary>> ->
      Src = extractMetric(Str),
      getMetrics(list_to_binary(string:substr(binary_to_list(Str),string:len("source=")+string:len(Src))),[Src | Source], MeasureList);
    _ ->
      case size(Str) of
        0 ->
          {Source, MeasureList};
        _ ->
          getMetrics(list_to_binary(string:substr(binary_to_list(Str),2)), Source, MeasureList)
      end
  end.

extractMetric(Str) ->
  SpaceIndex = string:str(binary_to_list(Str), " "),
  case SpaceIndex of
    0 -> string:substr(binary_to_list(Str), string:len("measure#")+1);
    _ -> string:substr(binary_to_list(Str), string:len("measure#")+1, SpaceIndex - string:len("measure#")-1)
  end.
