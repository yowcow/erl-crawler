-module(crawler_sup).

-behavior(supervisor).

-export([
    start_in_shell/0
]).
-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link(?MODULE, []).

start_in_shell() ->
    {ok, Pid} = start_link(),
    unlink(Pid).

init(_Args) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [
        #{
            id => crawler,
            start => {crawler, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [crawler]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
