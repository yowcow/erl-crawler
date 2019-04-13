-module(crawler).

-behavior(gen_server).

-export([
    start_link/0,
    stop/0,
    crawl/0,
    do/0
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(URLS, [
    "https://bo.beaconsco.com/app/feed/rss",
    "https://bo.beaconsco.com/hoge.txt",
    "https://bo.beaconsco.com/fuga.txt",
    "http://blog.livedoor.jp/yakiusoku/index.rdf"
]).
-define(INTERVAL, 30 * 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    process_flag(trap_exit, true),
    register(crawler_worker, spawn_link(fun() -> crawl_loop(?INTERVAL) end)),
    lager:info("crawler server initialized"),
    {ok, 0}.

handle_call(_, _, N) ->
    Items = crawl(),
    ok = log_item(Items),
    {reply, Items, N + 1}.

handle_cast(_, N) -> {noreply, N}.
handle_info(_, N) -> {noreply, N}.

terminate(_, _) ->
    process_flag(trap_exit, false),
    Pid = whereis(crawler_worker),
    unlink(Pid),
    exit(Pid, shutdown), % unlink and kill
    lager:info("crawler server terminated"),
    ok.

code_change(_, N, _) -> {ok, N}.

do() ->
    gen_server:call(?MODULE, [], 500).

crawl() ->
    lager:info("crawler started crawling"),
    Feeds = http_client:get_multi(?URLS),
    Items = feed_parser:parse_multi(Feeds),
    lager:info("crawler finished crawling"),
    Items.

crawl_loop(Interval) ->
    lager:info("crawler waiting"),
    timer:sleep(Interval),
    Items = crawl(),
    log_item(Items),
    crawl_loop(Interval).

log_item([]) -> ok;
log_item([{Title, Link} | Rem]) ->
    lager:info("title: ~ts, link: ~ts", [Title, Link]),
    log_item(Rem).
