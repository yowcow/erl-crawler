-module(crawler).

-behavior(gen_server).

-export([
    start_link/0,
    stop/0,
    crawl/0
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
    process_flag(trap_exit, true),
    register(notifier, spawn_link(fun() -> notifier(?INTERVAL) end)),
    lager:info("crawler server initialized"),
    {ok, 0}.

terminate(_, _) ->
    process_flag(trap_exit, false),
    Pid = whereis(notifier),
    unlink(Pid),
    exit(Pid, shutdown), % unlink and kill
    lager:info("crawler server terminated"),
    ok.

handle_call(crawl, _, N) ->
    lager:info("crawler started crawling"),
    Feeds = http_client:get_multi(?URLS),
    Items = feed_parser:parse_multi(Feeds),
    {Inserted, Skipped} = insert_item(Items),
    lager:info("crawler finished crawling (inserted: ~p, skipped: ~p)", [Inserted, Skipped]),
    {reply, #{ inserted => Inserted, skipped => Skipped }, N + 1}.

handle_cast(_, N) -> {noreply, N}.
handle_info(_, N) -> {noreply, N}.

code_change(_, N, _) -> {ok, N}.

crawl() ->
    gen_server:call(?MODULE, crawl, 1000).

insert_item(Items) ->
    Pid = db:start(),
    Ret = db:insert_ignore_multi(Pid, Items),
    db:stop(Pid),
    Ret.

notifier(Interval) ->
    lager:info("crawler notifier waiting"),
    timer:sleep(Interval),
    crawl(),
    notifier(Interval).
