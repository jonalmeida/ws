-module(ws).

%% API exports
-export([connect/2]).

%%====================================================================
%% API functions
%%====================================================================
connect(Uri, Port) ->
    {ok, Listen} = gen_tcp:connect(Uri, Port, [{active, true}]),
    gen_tcp:send(Listen, io:format("GET /\r
                                   Host: ~w\r
                                   Upgrade: websocet\r
                                   Connection: Upgrade\r
                                   Origin: http://example.com~n", [Uri])).

%%====================================================================
%% Internal functions
%%====================================================================
