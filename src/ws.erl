-module(ws).

-include("ws.hrl").

%% API exports
-export([connect/1,
         test/0,
         connect/2,
         request/2,
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
  connect(Url, 80, '/', ws_util:a_to_b(Url));
connect(#ws_url{host=Host, path=Path, port=Port, raw_path=Raw}) ->
  connect(Host, Port, Path, Raw).

connect(Host, Port, Path, Raw) ->
  case gen_tcp:connect(Host, Port, ?DEFAULT_OPTIONS) of
    {ok, Socket} ->
      #client{host=Host,
              path=Path,
              port=Port,
              socket=Socket,
              raw_path=Raw};
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
  request(get, Url).

-spec request(atom(), string()|atom()) -> binary().
request(get, Url) ->
  ParsedUrl = ws_url:parse(Url),
  Headers = ws_header:build([{"Host", ParsedUrl#ws_url.host},
                             {"GET", ParsedUrl#ws_url.path}]),
  SanitizedHeaders = ws_header:sanitize(Headers),
  io:format("~p~n", [ParsedUrl]),
  Client = connect(ParsedUrl),
  case send(Client, SanitizedHeaders) of
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

test() ->
  request(get, 'ws://localhost:8080').

%%====================================================================
%% Internal functions
%%====================================================================

