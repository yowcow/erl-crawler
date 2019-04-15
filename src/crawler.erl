-module(crawler).

-behavior(gen_server).

-export([
    start_link/0,
    stop/0,
    ping/0
]).
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-define(URLS, [
    "https://bo.beaconsco.com/app/feed/rss",
    "https://bo.beaconsco.com/hoge.txt",
    "https://bo.beaconsco.com/fuga.txt",
    "http://blog.livedoor.jp/yakiusoku/index.rdf"
]).
-define(INTERVAL, 60 * 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    erlang:send_after(?INTERVAL, self(), ping),
    lager:info("crawler server initialized"),
    {ok, 0}.

terminate(_, _) ->
    lager:info("crawler server terminated"),
    ok.

handle_call(ping, _, N) ->
    {Inserted, Skipped} = crawl(),
    {reply, #{ inserted => Inserted, skipped => Skipped }, N + 1}.

handle_cast(ping, N) ->
    crawl(),
    {noreply, N};
handle_cast(_, N) ->
    {noreply, N}.

handle_info(ping, N) ->
    crawl(),
    erlang:send_after(?INTERVAL, self(), ping), %% invoke ping again
    {noreply, N};
handle_info(_, N) ->
    {noreply, N}.

code_change(_, N, _) -> {ok, N}.

ping() ->
    gen_server:cast(?MODULE, ping).

crawl() ->
    lager:info("crawler started crawling"),
    Feeds = http_client:get_multi(?URLS),
    Items = feed_parser:parse_multi(Feeds),
    {Inserted, Skipped} = Ret = insert_item(Items),
    lager:info("crawler finished crawling (inserted: ~p, skipped: ~p)", [Inserted, Skipped]),
    Ret.

insert_item(Items) ->
    insert_item(db:start(), Items).

insert_item({ok, Pid}, Items) ->
    {ok, Ret} = db:insert_ignore_multi(Pid, Items),
    db:stop(Pid),
    Ret;
insert_item(Ret, _) ->
    lager:error("crawler failed connecting to db: ~p", [Ret]),
    {0, 0}.
