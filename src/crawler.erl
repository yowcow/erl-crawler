-module(crawler).

-behavior(gen_server).

-export([
    start_link/0,
    stop/0,
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
    register(worker, spawn_link(fun() -> do_and_wait(?INTERVAL) end)),
    lager:info("crawler started"),
    {ok, 0}.

handle_call(_, _, N) ->
    {reply, N, N + 1}.

handle_cast(_, N) -> {noreply, N}.
handle_info(_, N) -> {noreply, N}.

terminate(_, _) ->
    lager:info("crawler stopping ~p", [?MODULE]),
    process_flag(trap_exit, false),
    Pid = whereis(worker),
    unlink(Pid),
    exit(Pid, shutdown), % unlink and kill
    lager:info("crawler stopped"),
    ok.

code_change(_, N, _) -> {ok, N}.

do_and_wait(N) ->
    do(),
    timer:sleep(N),
    do_and_wait(N).

do() ->
    Feeds = http_client:get_multi(?URLS),
    Items = feed_parser:parse_multi(Feeds),
    ok = log_item(Items),
    lager:info("crawler finished crawling"),
    ok.

log_item([]) -> ok;
log_item([{Title, Link} | Rem]) ->
    lager:info("title: ~ts, link: ~ts", [Title, Link]),
    log_item(Rem).
