-module(ws_url).
-export([parse/1]).

-include("ws.hrl").

-spec parse(binary()|atom()|string()) -> ws_url().
parse(<<"ws://", Rest/binary>>) ->
  split_host_path(Rest, ws);
parse(<<"wss://", Rest/binary>>) ->
  split_host_path(Rest, wss);
parse(Raw) when is_binary(Raw) ->
  split_host_path(Raw);
parse(Raw) when is_atom(Raw) ->
  parse(ws_util:a_to_b(Raw));
parse(Raw) when is_list(Raw) ->
  parse(list_to_binary(Raw)).

-spec split_host_path(string()|binary(), string()) -> ws_url().
split_host_path(Url, Scheme) when is_list(Url) ->
  split_host_path(#ws_url{raw_path=list_to_binary(Url), scheme=Scheme});
split_host_path(Url, Scheme) when is_binary(Url) ->
  split_host_path(#ws_url{raw_path=Url, scheme=Scheme}).

-spec split_host_path(binary()|string()|ws_url()) -> ws_url().
split_host_path(Url) when is_binary(Url) ->
  split_host_path(#ws_url{raw_path=Url});
split_host_path(Url) when is_list(Url) ->
  split_host_path(#ws_url{raw_path=list_to_binary(Url)});
split_host_path(#ws_url{scheme=Scheme, raw_path=Raw}=Url) ->
  case binary:split(Raw, <<":">>) of
    [Raw] ->
      split_path(Raw, Scheme);
    [Host, PortPath] ->
      case binary:split(PortPath, <<"/">>) of
        [Port] ->
          Url#ws_url{host=ws_util:b_to_a(Host),
                     path='/',
                     port=ws_util:b_to_i(Port)};
        [Port, Path] ->
          Url#ws_url{host=ws_util:b_to_a(Host),
                     path=ws_util:b_to_a(Path),
                     port=ws_util:b_to_i(Port)}
      end
  end.

split_path(Raw, Scheme) ->
  case binary:split(Raw, <<"/">>) of
    [Addr] ->
      #ws_url{host=ws_util:b_to_a(Addr),
              path='/',
              scheme=Scheme};
    [Addr, <<>>] ->
      #ws_url{host=ws_util:b_to_a(Addr),
              path='/',
              scheme=Scheme};
    [Addr, Path] ->
      #ws_url{host=ws_util:b_to_a(Addr),
              path=ws_util:b_to_a(Path),
              scheme=Scheme}
  end.
