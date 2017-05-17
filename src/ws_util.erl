-module(ws_util).
-export([atom_to_binary/1]).

atom_to_binary(X) ->
  list_to_binary(atom_to_list(X)).
