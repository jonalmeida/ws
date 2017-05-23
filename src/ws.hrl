-define(DEFAULT_SCHEME, ws).

-define(WS_GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

-record(ws_url, {
          scheme=?DEFAULT_SCHEME :: string(),
          netloc :: atom(),
          user :: atom(),
          password :: atom(),
          port=80 :: number(),
          path :: atom(),
          query :: tuple(),
          raw_path :: binary()}).

-record(client, {
          netloc :: atom(),
          path :: atom(),
          port :: number(),
          query :: tuple(),
          key :: atom(),
          socket,
          raw_path :: binary()
         }).

-record(frame, {
          fin=true :: boolean(),
          rsv1=false :: boolean(),
          rsv2=false :: boolean(),
          rsv3=false :: boolean(),
          opcode=close :: atom(),
          mask=0 :: number(),
          payload :: binary()
         }).

-type ws_url() :: #ws_url{}.
-type client() :: #client{}.

-type net_error() :: inet:posix().

