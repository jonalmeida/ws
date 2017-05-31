-module(ws).

-include("ws.hrl").

%% API exports
-export([request/2,
         request/1,
         connect/2,
         send/2,
         test/0
        ]).

-define(DEFAULT_OPTIONS, [binary, {active, false}, {packet, raw}]).

%%====================================================================
%% API functions
%%====================================================================
-spec connect(string()|atom(), number()) -> client()|net_error().
connect(Url, Port) ->
  connect(#ws_url{netloc=Url, port=Port}).

-spec connect(string()|atom()|ws_url()) -> client()|net_error().
connect(Url) when is_list(Url) ->
  connect(list_to_atom(Url));
connect(Url) when is_atom(Url) ->
  connect(Url, 80, '/', ws_util:a_to_b(Url));
connect(#ws_url{netloc=Netloc, path=Path, port=Port, raw_path=Raw}) ->
  connect(Netloc, Port, Path, Raw).

connect(Netloc, Port, Path, Raw) ->
  case gen_tcp:connect(Netloc, Port, ?DEFAULT_OPTIONS) of
    {ok, Socket} ->
      #client{netloc=Netloc,
              path=Path,
              port=Port,
              socket=Socket,
              raw_path=Raw};
    {error, _Reason}=E ->
      E
  end.

-spec send_header(client(), binary()|[string()]) -> ok | net_error().
send_header(Client, Data) ->
  case Client#client.socket of
    undefined ->
      {error, "No connection has been made. Try using connect/1 first."};
    Socket ->
      gen_tcp:send(Socket, Data)
  end.

-spec request(string()|atom()) -> binary().
request(Url) ->
  request(get, Url).

-spec request(atom(), string()|atom()) -> binary().
request(get, Url) ->
  ParsedUrl = ws_url:parse(Url),
  Headers = ws_header:build([{"Host", atom_to_list(ParsedUrl#ws_url.netloc)},
                             {"GET", atom_to_list(ParsedUrl#ws_url.path)}]),
  SanitizedHeaders = ws_header:sanitize(Headers),
  io:format("~p~n", [ParsedUrl]),
  Client = connect(ParsedUrl),
  case send_header(Client, SanitizedHeaders) of
    ok ->
      {ok, RawResp} = gen_tcp:recv(Client#client.socket, 0),
      RespHeaders   = ws_header:parse(RawResp),
      AcceptKey     = ws_header:get_header(RespHeaders, "Sec-WebSocket-Accept"),
      SecKey        = ws_header:get_header(Headers, "Sec-WebSocket-Key"),
      case ws_header:check_accept(SecKey, AcceptKey) of
        true ->
          Client#client{key=SecKey};
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
  gen_tcp:send(Client#client.socket, binary, Data).

-spec send(client(), opcode(), string()|binary()) -> ok | net_error().
send(Client, Type, Data) ->
  EncData = ws_frame:encode_payload(Type, Data),
  case Client#client.socket of
    undefined ->
      {error, "No connection has been made. Try using connect/1 first."};
    Socket ->
      gen_tcp:send(Socket, EncData)
  end.


test() ->
  request(get, 'ws://localhost:8080').

%%====================================================================
%% Internal functions
%%====================================================================

