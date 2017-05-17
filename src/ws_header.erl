-module(ws_header).
-export([get_header/1,
         get_header/2,
         sanitize/1,
         build_headers/1,
         default_headers/0,
         check_accept/2,
         rand_bits/1,
         parse/1]).

-include("ws.hrl").

-spec build_headers([{string(), string()}]) -> [string()].
build_headers(L) ->
  lists:append(default_headers(), L).
  %Sanitized = sanitize_header(Full),
  %lists:flatten(Sanitized, ["\r\n"]). % Trailing '\r\n' as per RFC 2616 spec

-spec default_headers() -> [{string(), string()}].
default_headers() ->
  [get_header(connection),
   get_header(version),
   get_header(upgrade),
   get_header(protocol),
   get_header(key),
   get_header(extensions)].

-spec sanitize([{string(), string()}]) -> [string()].
sanitize(L) ->
  sanitize_header(L, [], []).

-spec sanitize_header([{string(), string()}], [string()], [string()]) -> [string()].
sanitize_header([], Ys, Get) ->
  lists:append([Get, Ys, ["\r\n"]]);
sanitize_header([{"GET", V}|KVs], Ys, _Get) ->
  sanitize_header(KVs, Ys, ["GET ", V, " HTTP/1.1\r\n"]);
sanitize_header([{K, V}|KVs], Ys, Get) ->
  sanitize_header(KVs, [K, ": ", V, "\r\n"|Ys], Get).

-spec get_header(atom()) -> {string(), string()}.
get_header(connection) ->
  {"Connection", "Upgrade"};
get_header(version) ->
  {"Sec-WebSocket-Version", "13"};
get_header(protocol) ->
  {"Sec-WebSocket-Protocol", "echo-protocol"};
get_header(extensions) ->
  {"Sec-WebSocket-Extensions", "permessage-deflate; client_max_window_bits"};
get_header(upgrade) ->
  {"Upgrade", "websocket"};
get_header(key) ->
  {"Sec-WebSocket-Key", binary_to_list(generate_key())};
get_header({K, V}) when is_atom(K), is_atom(V) ->
  {atom_to_list(K), atom_to_list(V)}.

-spec get_header(list(), string()) -> string().
get_header(Headers, Key) ->
  case lists:keysearch(Key, 1, Headers) of
    {value, {Key, Value}} ->
      Value;
    {value, {Key, Code, Reason}} ->
      {Code, Reason};
    false ->
      {error, no_val}
  end.

-spec check_accept(string(), string()) -> boolean().
check_accept(Key, Accept) ->
  Encoded = crypto:hash(sha, Key++?WS_GUID),
  Hashed = base64:encode(Encoded),
  Accept =:= binary_to_list(Hashed).

-spec generate_key() -> binary().
generate_key() ->
  base64:encode(rand_bits(128)).

-spec rand_bits(non_neg_integer()) -> binary().
rand_bits(Bits) ->
  Bytes = (Bits + 7) div 8,
  <<Result:Bits/bits, _/bits>> = crypto:strong_rand_bytes(Bytes),
  Result.

-spec parse(binary()) -> [{string(), string()}].
parse(RawHeader) ->
  parse(RawHeader, []).

-spec parse(binary(), [{string(), string()}]) -> [{string(), string()}].
parse([], ParsedHeaders) ->
  ParsedHeaders;
parse(RawHeader, ParsedHeaders) ->
  case binary:split(RawHeader, <<"\r\n">>) of
    [Header, <<"\r\n">>] ->
      [parse_header(Header)|ParsedHeaders];
    [Header, Next] ->
      Parsed = parse_header(Header),
      parse(Next, [Parsed|ParsedHeaders])
  end.

-spec parse_header(binary()) -> {string(), string()}.
parse_header(<<"HTTP/1.1 ", Status/binary>>) ->
  case binary:split(Status, <<" ">>) of
    [Code, Reason] ->
      {"HTTP/1.1", binary_to_list(Code), binary_to_list(Reason)};
    [Other] ->
      {"HTTP/1.1", binary_to_list(Other)}
  end;
parse_header(Header) ->
  case binary:split(Header, <<": ">>) of
    [<<>>] ->
      no_msg;
    [Name, Value] ->
      {binary_to_list(Name), binary_to_list(Value)};
    [Header] ->
      {binary_to_list(Header)}
  end.

