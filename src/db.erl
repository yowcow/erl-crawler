-module(db).

-export([
    start/0,
    stop/1,
    insert_ignore_multi/2
]).

start() ->
    {ok, DSN} = crawler_app:config(db),
    mysql:start_link(DSN).

stop(Pid) ->
    mysql:stop(Pid).

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
    {ok, {Inserted, Skipped}};
insert_ignore_multi(Pid, [Item | L], Inserted, Skipped) ->
    case insert_ignore(Pid, Item) of
        ok -> insert_ignore_multi(Pid, L, Inserted + 1, Skipped);
        _  -> insert_ignore_multi(Pid, L, Inserted, Skipped + 1)
    end.
