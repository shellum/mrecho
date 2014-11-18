-module(mrecho_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, _Body, Req2} = cowboy_req:body(Req),
  StrippedBody = binary:replace(binary:replace(_Body, <<"\n">>,<<" ">>,[global]),<<"\\n">>,<<" ">>,[global]),
  io:format("body: ~p\n",[StrippedBody]),
  EventList = getMetrics(StrippedBody, list_to_binary("unknown"), []),

  case length(element(2,EventList)) of
    0 -> {ok, Req2, State};
    _X ->

  io:format("event list: ~p\n",[EventList]),
  FormattedEventList = element(2,EventList),
  Event = [{<<"gauges">>, FormattedEventList}],
  EncodedEvent = jsx:encode(Event),
  io:format("event json: ~p\n\n",[EncodedEvent]),

  ResponseCode = send_metric(EncodedEvent),
  Resp = "{code:'"++integer_to_list(ResponseCode)++"'}",

  {ok, Req3} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Resp, Req2),
  {ok, Req3, State}
end.

send_metric(Events) ->
  Headers = [{"Authorization", "Basic " ++ base64:encode_to_string(credentials())}],
  case httpc:request(post, {libratoUrl(), Headers, "application/json", Events}, [], []) of
    {ok, {{_, 200, _}, _Header, _NewBody}} ->
      200;
    {ok, {{_, I, _}, _Header, _NewBody}} when is_integer(I) ->
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
    <<"host=", _Rest/binary>> ->
      TagLen = string:len("host="),
      Src = extractMetric(Str, TagLen),
      io:format("extractMetricz: ~p", [Src]),
      ParsedSrc = case getLastPart(Src) of
        "test" -> "dev";
        "dev" -> "dev";
        "development" -> "dev";
        "qa" -> "qa";
        "production" -> "production";
        "prod" -> "production";
        _ -> Src
      end,
      io:format("ParsedSrc: ~p",[ParsedSrc]),
      getMetrics(list_to_binary(string:substr(binary_to_list(Str),TagLen+string:len(Src))),list_to_binary(ParsedSrc), MeasureList);
    <<"status=", _Rest/binary>> ->
      TagLen = string:len("status="),
      StatusCode = extractMetric(Str, TagLen),
      K = list_to_binary("status." ++ StatusCode),
      V = <<"1">>,
      NewStart = string:len(StatusCode)+TagLen,
      getMetrics(list_to_binary(string:substr(binary_to_list(Str),NewStart)), Source, [[{name, K},{value,V},{source,Source}] | MeasureList]);
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

getLastPart(Str) ->
  Tokens = string:tokens(Str, "-"),
  io:format("Tokens: ~p",[Tokens]),
  List = lists:reverse(Tokens),
  io:format("List: ~p",[List]),
  [H|_] = List,
  H2 = string:substr(H, 1,string:str(H,".")-1),
  io:format("H2: ~p",[H2]),
  H2.

libratoUrl() ->
  case os:getenv("LIBRATO_URL") of
      false -> "";
      L -> L ++ ""
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
