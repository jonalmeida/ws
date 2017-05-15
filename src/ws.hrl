-define(DEFAULT_SCHEME, "ws").

-define(WS_GUID, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>).

-record(ws_url, {
          scheme=?DEFAULT_SCHEME,
          host,
          user,
          password,
          port=80,
          path,
          query,
          raw_path}).

-record(client, {
          host,
          path,
          port,
          query,
          socket,
          raw_path
         }).

-type ws_url() :: #ws_url{}.
-type client() :: #client{}.

-type net_error() :: inet:posix().

