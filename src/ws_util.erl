-module(ws_util).
-export([a_to_b/1,
  b_to_a/1,
  b_to_i/1,
  rand_bits/1]).

a_to_b(X) ->
  atom_to_binary(X, utf8).

b_to_a(X) ->
  binary_to_atom(X, utf8).

b_to_i(X) ->
  binary_to_integer(X).

-spec rand_bits(non_neg_integer()) -> binary().
rand_bits(Bits) ->
  Bytes = (Bits + 7) div 8,
  <<Result:Bits/bits, _/bits>> = crypto:strong_rand_bytes(Bytes),
  Result.
