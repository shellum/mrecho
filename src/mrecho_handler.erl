-module(mrecho_handler).

-include_lib("eunit/include/eunit.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([get_metrics/3]).

-define(INDEX_SOURCE, 1).
-define(INDEX_METRIC, 2).
-define(METRIC_DELIMITER, " ").
-define(AGGREGATION_SECONDS, <<"60">>).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, _Body, Req2} = cowboy_req:body(Req),
  StrippedBody = binary:replace(binary:replace(_Body, <<"\n">>,<<" ">>,[global]),<<"\\n">>,<<" ">>,[global]),
  io:format("body: ~p\n",[StrippedBody]),
  EventList = get_metrics(StrippedBody, list_to_binary("unknown"), []),

  case length(element(?INDEX_METRIC,EventList)) of
    0 ->
      {ok, Req2, State};
    _X ->
      FormattedEventList = element(?INDEX_METRIC,EventList),
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
  case httpc:request(post, {librato_url(), Headers, "application/json", Events}, [], []) of
    {ok, {{_, 200, _}, _Header, _NewBody}} ->
      200;
    {ok, {{_, I, _}, _Header, _NewBody}} when is_integer(I) ->
      I;
    {error, socket_closed_remotely} ->
      {error, <<"No socket">>};
    _ -> 400
  end.

terminate(_Reason, _Req, _State) ->
  ok.

get_metrics(Str,Source,MeasureList) ->
  case Str of
    <<"measure#",_Rest/binary>> ->
      handle_measurement(Str, "measure#", Source, MeasureList);
    <<"count#",_Rest/binary>> ->
      handle_measurement(Str, "count#", Source, MeasureList);
    <<"source=", _Rest/binary>> ->
      handle_host(Str, "source=", MeasureList);
    <<"host=", _Rest/binary>> ->
      handle_host(Str, "host=", MeasureList);
    <<"status=", _Rest/binary>> ->
      handle_status(Str, "status=", Source, MeasureList);
    _ ->
      case size(Str) of
        0 ->
          {Source, MeasureList};
        _ ->
          get_metrics(list_to_binary(string:substr(binary_to_list(Str),2)), Source, MeasureList)
      end
  end.

handle_host(Str, IdString, MeasureList) ->
  TagLen = string:len(IdString),
  Src = extract_metric(Str, TagLen),
  get_metrics(list_to_binary(string:substr(binary_to_list(Str),TagLen+string:len(Src))),list_to_binary(Src), MeasureList).

handle_measurement(Str, IdString, Source, MeasureList) ->
  TagLen = string:len(IdString),
  KVPair = extract_metric(Str, TagLen),
  [K,V] = binary:split(list_to_binary(KVPair), [<<"=">>],[]),
  TunedV = binary:replace(V,<<"ms">>,<<"">>),
  NewStart = size(K)+1+size(V)+TagLen,
  get_metrics(list_to_binary(string:substr(binary_to_list(Str),NewStart)), Source, [[{name, K},{value,TunedV},{source,Source}] | MeasureList]).

handle_status(Str, IdString, Source, MeasureList) ->
  TagLen = string:len(IdString),
  StatusCode = extract_metric(Str, TagLen),
  K = list_to_binary("status." ++ StatusCode),
  V = <<"1">>,
  NewStart = string:len(StatusCode)+TagLen,
  get_metrics(list_to_binary(string:substr(binary_to_list(Str),NewStart)), Source, [[{name, K},{value,V},{source,Source},{period,?AGGREGATION_SECONDS}] | MeasureList]).

extract_metric(Str, TagLen) ->
  MetricDelimiterIndex = string:str(binary_to_list(Str), ?METRIC_DELIMITER),
  LengthToSkip = TagLen+string:len(?METRIC_DELIMITER),
  case MetricDelimiterIndex of
    0 -> string:substr(binary_to_list(Str), LengthToSkip);
    _ -> string:substr(binary_to_list(Str), LengthToSkip, MetricDelimiterIndex - LengthToSkip)
  end.

librato_url() ->
  case os:getenv("LIBRATO_URL") of
      false -> "";
      U -> U
  end.

credentials() ->
  Email = case os:getenv("LIBRATO_EMAIL") of
      false -> "";
      E -> E
  end,
  ApiKey = case os:getenv("LIBRATO_API_KEY") of
      false -> "";
      A -> A
  end,
  list_to_binary(Email ++ ":" ++ ApiKey).


extract_metric_test() ->
  TestMetric = <<"status=">>,
  TestValue = <<"304">>,
  RestOfLine = <<" metric#get.db=50ms blah blah">>,
  Size = size(TestMetric),
  Metric = extract_metric(<<TestMetric/binary,TestValue/binary,RestOfLine/binary>>,Size),
  Metric = "304".

get_metrics_test() ->
  Result = get_metrics(<<"at=info method=GET path=\"/invites/075e64a1ca86f9fcb392f8abe9c914248d7d3842\" host=oc-peerapi-qa.herokuapp.com request_id=aa0ffc40-a3f3-4af8-8914-171ebecbddfe fwd=\"65.121.23.138,54.82.146.82\" dyno=web.2 connect=2ms service=11ms status=200 bytes=779">>, <<"unknown">>,[]),
  {<<"oc-peerapi-qa.herokuapp.com">>,[[{name,<<"status.200">>},{value,<<"1">>},{source,<<"oc-peerapi-qa.herokuapp.com">>},{period,<<"60">>}]]} = Result.
