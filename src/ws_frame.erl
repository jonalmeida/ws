-module(ws_frame).
-export([mask/3, generate_mask_key/0]).

mask(Key,Data,Accu) ->
  case Data of
    <<A:32,Rest/binary>> ->
      C = binary:encode_unsigned(A bxor Key),
      mask(Key,Rest,<<Accu/binary,C/binary>>);
    <<A:24>> ->
      <<B:24, _:8>> = binary:encode_unsigned(Key),
      C = binary:encode_unsigned(A bxor B),
      <<Accu/binary,C/binary>>;
    <<A:16>> ->
      <<B:16, _:16>> = binary:encode_unsigned(Key),
      C = binary:encode_unsigned(A bxor B),
      <<Accu/binary,C/binary>>;
    <<A:8>> ->
      <<B:8, _:24>> = binary:encode_unsigned(Key),
      C = binary:encode_unsigned(A bxor B),
      <<Accu/binary,C/binary>>;
    <<>> ->
      Accu
  end.

generate_mask_key() ->
  ws_util:rand_bits(32).
