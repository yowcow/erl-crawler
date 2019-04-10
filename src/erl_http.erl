-module(erl_http).

-export([
    main/1
]).

-define(URLS, [
    "https://bo.beaconsco.com/app/feed/rss",
    "https://bo.beaconsco.com/hoge.txt",
    "http://blog.livedoor.jp/yakiusoku/index.rdf",
    "https://www3.nhk.or.jp/rss/news/cat0.xml"
]).

-define(APPS, [crypto, asn1, public_key, ssl]).

start_app([]) ->
    inets:start(),
    lager:start(),
    ok;
start_app([App | Rem]) ->
    application:start(App),
    start_app(Rem).

main(_Args) ->
    ok = start_app(?APPS),
    Feeds = http_client:get_multi(?URLS),
    _ = feed_parser:parse_multi(Feeds),
    ok.
