-module(ws).

-include("ws.hrl").

%% API exports
-export([connect/1,
         test/0,
         connect/2,
         request/3,
         request/1]).

-define(DEFAULT_OPTIONS, [binary, {active, false}, {packet, raw}]).

%%====================================================================
%% API functions
%%====================================================================
-spec connect(string()|atom(), number()) -> client()|net_error().
connect(Url, Port) ->
  connect(#ws_url{host=Url, port=Port}).

-spec connect(string()|atom()|ws_url()) -> client()|net_error().
connect(Url) when is_list(Url) ->
  connect(list_to_atom(Url));
connect(Url) when is_atom(Url) ->
  case gen_tcp:connect(Url, 80, ?DEFAULT_OPTIONS) of
    {ok, Socket} ->
      #client{host=Url,
              socket=Socket,
              raw_path=ws_util:atom_to_binary(Url)};
    {error, _Reason}=E ->
      E
  end;
connect(#ws_url{host=Host, path=Path}=Url) ->
  Port = Url#ws_url.port,
  case gen_tcp:connect(Host, Port, ?DEFAULT_OPTIONS) of
    {ok, Socket} ->
      #client{host=Host,
              path=Path,
              port=Port,
              socket=Socket,
              raw_path=Url#ws_url.raw_path};
    {error, _Reason}=E ->
      E
  end.

-spec send(client(), binary()|[string()]) -> ok | {error, closed|net_error()}.
send(Client, Data) ->
  case Client#client.socket of
    undefined ->
      {error, "No connection has been made. Try using connect/1 first."};
    Socket ->
      gen_tcp:send(Socket, Data)
  end.

-spec request(string()|atom()) -> binary().
request(Url) ->
  request(get, Url, 80).

-spec request(atom(), string()|atom(), number()) -> binary().
request(get, Url, Port) ->
%  Header = ["GET /ip HTTP/1.0\r\n",
%     %"Connection: keep-alive\r\n",
%     %"Keep-Alive: 300\r\n",
%     "Host: ", "Hostname", "\r\n",
%     "Upgrade: websocket\r\n",
%     "Connection: Upgrade\r\n",
%     "Sec-WebSocket-Version: 13\r\n",
%     "Sec-WebSocket-Protocol: echo-protocol\r\n",
%     "Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits\r\n",
%     "Sec-WebSocket-Key:dGhlIHNhbXBsZSBub25jZQ==\r\n",
%     "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\r\n"],
  ParsedUrl = ws_url:parse_url(Url),
  Headers = ws_header:build_headers([{"Host", ParsedUrl#ws_url.host},
                                     {"GET", ParsedUrl#ws_url.path}]),
  SanitizedHeaders = ws_header:sanitize(Headers),
  Client = connect(Url, Port),
  send(Client, SanitizedHeaders),
  {ok, RawResp} = gen_tcp:recv(Client#client.socket, 0),
  RespHeaders = ws_header:parse(RawResp),
  AcceptKey = ws_header:get_header(RespHeaders, "Sec-WebSocket-Accept"),
  SecKey = ws_header:get_header(Headers, "Sec-WebSocket-Key"),
  case ws_header:check_accept(SecKey, AcceptKey) of
    true ->
      Client#client{key=SecKey};
    false ->
      error(key_auth_fail)
  end.

test() ->
  request(get, localhost, 8080).

%%====================================================================
%% Internal functions
%%====================================================================

