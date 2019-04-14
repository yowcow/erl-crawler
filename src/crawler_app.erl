-module(crawler_app).

-behavior(application).

-export([
    start/2,
    stop/1,
    config/1
]).

start(_Type, _) ->
    lager:start(),
    crawler_sup:start_link().

stop(_State) ->
    lager:stop(),
    ok.

config(Key) ->
    application:get_env(crawler, Key).
