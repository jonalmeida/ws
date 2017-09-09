-module(ws).

-include("ws.hrl").

%% API exports
-export([connect/2, %TODO: Remove later
  connect/1, %TODO: Remove later
  connect_host/2, %TODO: Remove later
  send/3,
  send/2,
  close/1,
  ping/1,
  test/0
]).

-define(DEFAULT_OPTIONS, [binary, {active, false}, {packet, raw}]).

%%====================================================================
%% API functions
%%====================================================================
-spec connect(string()|atom()) -> binary().
connect(Url) ->
  connect(get, Url).

-spec connect(atom(), string()|atom()) -> binary().
connect(get, Url) ->
  ParsedUrl = ws_url:parse(Url),
  Headers = ws_header:build([{"Host", atom_to_list(ParsedUrl#ws_url.netloc)},
    {"GET", atom_to_list(ParsedUrl#ws_url.path)}]),
  SanitizedHeaders = ws_header:sanitize(Headers),
  io:format("~p~n", [ParsedUrl]),
  Client = connect_host(ParsedUrl),
  case send_header(Client, SanitizedHeaders) of
    ok ->
      {ok, RawResp} = gen_tcp:recv(Client#client.socket, 0),
      RespHeaders = ws_header:parse(RawResp),
      AcceptKey = ws_header:get_header(RespHeaders, "Sec-WebSocket-Accept"),
      SecKey = ws_header:get_header(Headers, "Sec-WebSocket-Key"),
      case ws_header:check_accept(SecKey, AcceptKey) of
        true ->
          % Start using the mailbox from now on.
          inet:setopts(Client#client.socket, [{active, true}]),
          Client#client{key = SecKey};
        false ->
          error(key_auth_fail)
      end;
    E ->
      E
  end.

-spec send(client(), string()|binary()) -> ok | {error, string()}.
send(Client, Data) when is_list(Data) ->
  send(Client, text, list_to_binary(Data));
send(Client, Data) when is_binary(Data) ->
  send(Client, binary, Data).

-spec send(client(), opcode(), string()|binary()) -> ok | net_error().
send(Client, Type, Data) ->
  EncPayload = ws_frame:encode_payload({Type, Data}),
  case Client#client.socket of
    undefined ->
      {error, "No connection has been made. Try using connect/1 first."};
    Socket ->
      gen_tcp:send(Socket, EncPayload)
  end.

-spec close(client()) -> ok.
close(Client) ->
  send_type(Client, close).

-spec ping(client()) -> ok.
ping(Client) ->
  send_type(Client, ping).

-spec send_type(client(), opcode()) -> ok.
send_type(Client, Type) ->
  EncPayload = ws_frame:encode_payload(Type),
  case Client#client.socket of
    undefined ->
      ok;
    Socket ->
      gen_tcp:send(Socket, EncPayload)
  end.

test() ->
  connect(get, 'ws://localhost:8080').

%%====================================================================
%% Internal functions
%%====================================================================
-spec connect_host(string()|atom(), number()) -> client()|net_error().
connect_host(Url, Port) ->
  connect_host(#ws_url{netloc = Url, port = Port}).

-spec connect_host(string()|atom()|ws_url()) -> client()|net_error().
connect_host(Url) when is_list(Url) ->
  connect_host(list_to_atom(Url));
connect_host(Url) when is_atom(Url) ->
  connect_host(Url, 80, '/', ws_util:a_to_b(Url));
connect_host(#ws_url{netloc = Netloc, path = Path, port = Port, raw_path = Raw}) ->
  connect_host(Netloc, Port, Path, Raw).

connect_host(Netloc, Port, Path, Raw) ->
  case gen_tcp:connect(Netloc, Port, ?DEFAULT_OPTIONS) of
    {ok, Socket} ->
      #client{netloc = Netloc,
        path = Path,
        port = Port,
        socket = Socket,
        raw_path = Raw};
    {error, _Reason} = E ->
      E
  end.


-spec send_header(client(), binary()|[string()]) -> ok | net_error().
send_header(Client, Data) ->
  case Client#client.socket of
    undefined ->
      {error, "No connection has been made. Try using connect_host/1 first."};
    Socket ->
      gen_tcp:send(Socket, Data)
  end.

