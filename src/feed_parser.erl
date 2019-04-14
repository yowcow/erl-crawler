-module(feed_parser).

-include_lib("xmerl/include/xmerl.hrl").

-export([
    parse_multi/1
]).

parse_multi(Feeds) ->
    parse_multi(Feeds, []).

parse_multi([], Parsed) -> Parsed;
parse_multi([Feed | Rem], Parsed) ->
    {Xml, _} = xmerl_scan:string(binary_to_list(Feed)),
    Items = xmerl_xpath:string("//item", Xml),
    parse_multi(Rem, parse_item(Items, Parsed)).

parse_item([], Parsed) -> Parsed;
parse_item([Item | Rem], Parsed) ->
    Title = content_value(xmerl_xpath:string("//title", Item)),
    Link = content_value(xmerl_xpath:string("//link", Item)),
    parse_item(Rem, [{string:trim(Title), string:trim(Link)} | Parsed]).

content_value([Elem | _]) ->
    [Text | _] = Elem#xmlElement.content,
    Text#xmlText.value.
