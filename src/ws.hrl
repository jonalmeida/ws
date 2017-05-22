-define(DEFAULT_SCHEME, ws).

-define(WS_GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

-record(ws_url, {
          scheme=?DEFAULT_SCHEME :: string(),
          host :: atom(),
          user :: atom(),
          password :: atom(),
          port=80 :: number(),
          path :: atom(),
          query :: tuple(),
          raw_path :: binary()}).

-record(client, {
          host :: atom(),
          path :: atom(),
          port :: number(),
          query :: tuple(),
          key :: atom(),
          socket,
          raw_path :: binary()
         }).

-record(frame, {
          fin :: boolean(),
          rsv1 :: boolean(),
          rsv2 :: boolean(),
          rsv3 :: boolean(),
          opcode :: number(),
          mask=0 :: number(),
          payload :: binary()
         }).

-type ws_url() :: #ws_url{}.
-type client() :: #client{}.

-type net_error() :: inet:posix().

