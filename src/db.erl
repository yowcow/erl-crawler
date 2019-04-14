-module(db).

-export([
    start/0,
    stop/1,
    insert_ignore_multi/2
]).

start() ->
    {ok, {Host, Database, User, Password}} = crawler_app:config(db),
    {ok, Pid} = mysql:start_link([
        {host, Host},
        {user, User},
        {password, Password},
        {database, Database}
    ]),
    lager:info("db connected: ~p", [Pid]),
    Pid.

stop(Pid) ->
    Ret = mysql:stop(Pid),
    lager:info("db disconnected: ~p", [Pid]),
    Ret.

count_by_link(Pid, Link) ->
    {ok, _, [[Count]]} = mysql:query(Pid, "SELECT count(*) FROM item WHERE link = ?", [Link]),
    {ok, Count}.

insert(Pid, {Title, Link}) ->
    _ = mysql:query(Pid, "INSERT INTO item (link, title) VALUES (?, ?)", [Link, Title]),
    ok.

insert_ignore(Pid, {Title, Link}) ->
    case count_by_link(Pid, Link) of
        {ok, 0} ->
            insert(Pid, {Title, Link});
        _ ->
            skip
    end.

insert_ignore_multi(Pid, Items) ->
    insert_ignore_multi(Pid, Items, 0, 0).

insert_ignore_multi(_, [], Inserted, Skipped) ->
    {Inserted, Skipped};
insert_ignore_multi(Pid, [Item | L], Inserted, Skipped) ->
    case insert_ignore(Pid, Item) of
        ok -> insert_ignore_multi(Pid, L, Inserted + 1, Skipped);
        _  -> insert_ignore_multi(Pid, L, Inserted, Skipped + 1)
    end.