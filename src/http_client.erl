-module(http_client).

-export([
    get_multi/1
]).

-define(TIMEOUT_MSEC, 300).

get_multi(Urls) ->
    {Count, Reqs} = request(Urls),
    retrieve(Count, Reqs).

retrieve(Count, Reqs) ->
    retrieve(Count, Reqs, []).

retrieve(0, _, Feeds) -> Feeds;
retrieve(Count, Reqs, Feeds) ->
    receive
        {http, {ReqId, {{_, 200, _}, _, Body}}} ->
            Url = proplists:get_value(ReqId, Reqs),
            lager:info("Got response OK: ~ts", [Url]),
            retrieve(Count - 1, Reqs, [Body | Feeds]);
        {http, {ReqId, {{_, Code, _}, _, _}}} ->
            Url = proplists:get_value(ReqId, Reqs),
            lager:info("Got response NG: ~ts (~p)", [Url, Code]),
            retrieve(Count - 1, Reqs, Feeds)
        after ?TIMEOUT_MSEC ->
            lager:info("Giving up response for ~p requests", [Count]),
            retrieve(0, Reqs, Feeds)
    end.

request(Urls) ->
    request(Urls, [], 0).

request([], Reqs, Count) -> {Count, Reqs};
request([Url | Rem], Reqs, Count) ->
    {ok, ReqId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
    request(Rem, [{ReqId, Url} | Reqs], Count + 1).
