-module(cache_server_http).

-export([init/2]).

-include("../include/cache_server.hrl").

init(#{method := <<"POST">>} = Req0, Opts) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req0),
    DataMap = jsx:decode(Body, [return_maps]),
    ResultJsx = jsx:encode(request_fun(DataMap)),
    Req = cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain">>
    }, ResultJsx, Req0),
    {ok, Req, Opts}.


request_fun(#{action := <<"insert">>} = DataMap) ->
    Key = maps:get(<<"key">>, DataMap),
    Value = maps:get(<<"value">>, DataMap),
    TTL = maps:get(<<"key">>, DataMap, ?TTL),
    ok = cache_server:insert(?TABLENAME, Key, Value, TTL),
    [{<<"result">>, <<"ok">>}];

request_fun(#{action := <<"lookup">>} = DataMap) ->
    Key = maps:get(<<"key">>, DataMap),
    Result = cache_server:lookup(?TABLENAME, Key),
    Res2 = case Result of
        {ok, Value} ->
            Value;
        X ->
            X
    end,
    [{<<"result">>, Res2}];

request_fun(#{action := <<"lookup_by_date">>} = DataMap) ->
    DateFromBin = maps:get(<<"date_from">>, DataMap),
    DateToBin = maps:get(<<"date_to">>, DataMap),
    DateFrom = help_funs:date_binary_to_erlang_time(DateFromBin),
    DateTo = help_funs:date_binary_to_erlang_time(DateToBin),
    {ok, KeyValueList} = cache_server:lookup_by_date(?TABLENAME, DateFrom, DateTo),
    Result = help_funs:proplist_to_result(KeyValueList),
    [{<<"result">>, Result}];

request_fun(DataMap) ->
    [{<<"result">>, <<"request not found">>}].






