-module(mrecho_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, _Body, Req2} = cowboy_req:body(Req),
  io:format("body: ~p",[_Body]),
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
  Headers = [{"Authorization", "Basic " ++ base64:encode_to_string(credentials())}],
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
      TagLen = string:len("measure#"),
      KVPair = extractMetric(Str, TagLen),
      [K,V] = binary:split(list_to_binary(KVPair), [<<"=">>],[]),
      TunedV = binary:replace(V,<<"ms">>,<<"">>),
      NewStart = size(K)+1+size(V)+TagLen,
      getMetrics(list_to_binary(string:substr(binary_to_list(Str),NewStart)), Source, [[{name, K},{value,TunedV},{source,Source}] | MeasureList]);
    <<"count#",_Rest/binary>> ->
      TagLen = string:len("count#"),
      KVPair = extractMetric(Str, TagLen),
      [K,V] = binary:split(list_to_binary(KVPair), [<<"=">>],[]),
      NewStart = size(K)+1+size(V)+TagLen,
      getMetrics(list_to_binary(string:substr(binary_to_list(Str),NewStart)), Source, [[{name, K},{value,V},{source,Source}] | MeasureList]);
    <<"source=", _Rest/binary>> ->
      TagLen = string:len("source="),
      Src = extractMetric(Str, TagLen),
      getMetrics(list_to_binary(string:substr(binary_to_list(Str),TagLen+string:len(Src))),list_to_binary(Src), MeasureList);
    _ ->
      case size(Str) of
        0 ->
          {Source, MeasureList};
        _ ->
          getMetrics(list_to_binary(string:substr(binary_to_list(Str),2)), Source, MeasureList)
      end
  end.

extractMetric(Str, TagLen) ->
  SpaceIndex = string:str(binary_to_list(Str), " "),
  case SpaceIndex of
    0 -> string:substr(binary_to_list(Str), TagLen+1);
    _ -> string:substr(binary_to_list(Str), TagLen+1, SpaceIndex - TagLen-1)
  end.

credentials() ->
    Email = case os:getenv("LIBRATO_EMAIL") of
        false -> "";
        E -> E ++ ""
    end,
    ApiKey = case os:getenv("LIBRATO_API_KEY") of
        false -> "";
        A -> A
    end,
    list_to_binary(Email ++ ":" ++ ApiKey).
