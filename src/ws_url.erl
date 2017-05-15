-module(ws_url).
-export([parse_url/1]).

-include("ws.hrl").

-spec parse_url(binary()|atom()|string()) -> ws_url().
parse_url(<<"ws://", Rest/binary>>) ->
    split_host_path(Rest, "ws");
parse_url(<<"wss://", Rest/binary>>) ->
    split_host_path(Rest, "wss");
parse_url(Raw) when is_atom(Raw) ->
    parse_url(list_to_binary(atom_to_list(Raw)));
parse_url(Raw) when is_list(Raw) ->
    parse_url(list_to_binary(Raw)).

-spec split_host_path(binary()|string()|ws_url()) -> ws_url().
split_host_path(Url) when is_binary(Url) ->
    split_host_path(#ws_url{raw_path=binary_to_list(Url)});
split_host_path(Url) when is_list(Url) ->
    split_host_path(#ws_url{raw_path=Url});
split_host_path(#ws_url{scheme=Scheme, raw_path=Raw}) ->
    case binary:split(Raw, <<"/">>) of
        [Addr] ->
            #ws_url{host=Addr, path="/", scheme=Scheme};
        [Addr, <<>>] ->
            #ws_url{host=Addr, path="/", scheme=Scheme};
        [Addr, Path] ->
            #ws_url{host=Addr, path=Path, scheme=Scheme}
    end.

-spec split_host_path(string()|binary(), string()) -> ws_url().
split_host_path(Url, Scheme) when is_list(Url) ->
    split_host_path(#ws_url{raw_path=list_to_binary(Url), scheme=Scheme});
split_host_path(Url, Scheme) when is_binary(Url) ->
    split_host_path(#ws_url{raw_path=Url, scheme=Scheme}).
