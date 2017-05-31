-module(ws_frame).
-export([mask/3,
         encode_payload/2,
         generate_mask_key/0]).

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

encode_payload(Type, Payload) ->
  MaskingKeyBin = generate_mask_key(),
  <<MaskingKey:32>> = MaskingKeyBin,
  OpCode = atom_to_opcode(Type),
  PayloadBitLen = payload_length_to_binary(iolist_size(Payload)),
  MaskedPayload = mask(MaskingKey, Payload, <<>>),
  <<1:1, 0:3, OpCode:4, 1:1,  % Fin, Op1, Op2, Op3, Mask
    PayloadBitLen/bits,       % Payload length
    MaskingKeyBin/bits,       % Masking key
    MaskedPayload/binary>>.   % Masked payload

payload_length_to_binary(Len) when Len =<125 ->
    << Len:7 >>;
payload_length_to_binary(Len) when Len =< 16#ffff ->
    << 126:7, Len:16 >>;
payload_length_to_binary(Len) when Len =< 16#7fffffffffffffff ->
<< 127:7, Len:64 >>.

atom_to_opcode(continue) -> 0;
atom_to_opcode(text) -> 1;
atom_to_opcode(binary) -> 2;
atom_to_opcode(close) -> 8;
atom_to_opcode(ping) -> 9;
atom_to_opcode(pong) -> 10.

opcode_to_atom(0) -> continue;
opcode_to_atom(1) -> text;
opcode_to_atom(2) -> binary;
opcode_to_atom(8) -> close;
opcode_to_atom(9) -> ping;
opcode_to_atom(10) -> pong.
