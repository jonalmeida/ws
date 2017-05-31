-module(ws_frame).
-export([mask/3,
         encode_payload/2,
         generate_mask_key/0]).

-include("ws.hrl").

%% @doc Creates a xor'd output using the masking key.
-spec mask(number(), binary(), binary()) -> binary().
mask(Key,Data,Accu) ->
  case Data of
    <<A:32, Rest/binary>> ->
      C = binary:encode_unsigned(A bxor Key),
      mask(Key,Rest,<<Accu/binary, C/binary>>);
    <<A:24>> ->
      <<B:24, _:8>> = binary:encode_unsigned(Key),
      C = binary:encode_unsigned(A bxor B),
      <<Accu/binary, C/binary>>;
    <<A:16>> ->
      <<B:16, _:16>> = binary:encode_unsigned(Key),
      C = binary:encode_unsigned(A bxor B),
      <<Accu/binary, C/binary>>;
    <<A:8>> ->
      <<B:8, _:24>> = binary:encode_unsigned(Key),
      C = binary:encode_unsigned(A bxor B),
      <<Accu/binary, C/binary>>;
    <<>> ->
      Accu
  end.

%% @doc Generates a 32-bit masking key as per spec.
-spec generate_mask_key() -> binary().
generate_mask_key() ->
  ws_util:rand_bits(32).

%% @doc Formats the binary payload with a new generated masking key,
%%      while also setting the correct opcode.
-spec encode_payload(opcode(), binary()) -> binary().
encode_payload(Type, Payload) ->
  MaskingKeyBin     = generate_mask_key(),
  <<MaskingKey:32>> = MaskingKeyBin,
  OpCode        = atom_to_opcode(Type),
  PayloadBitLen = payload_length_to_binary(iolist_size(Payload)),
  MaskedPayload = mask(MaskingKey, Payload, <<>>),
  <<1:1, 0:3, OpCode:4, 1:1,  % Fin, Op1, Op2, Op3, Mask
    PayloadBitLen/bits,       % Payload length
    MaskingKeyBin/bits,       % Masking key
    MaskedPayload/binary>>.   % Masked payload

%% @doc Creates correctly padded payload len in binary format dependant
%%      on the value.
-spec payload_length_to_binary(number()) -> binary().
payload_length_to_binary(Len) when Len =<125 ->
  << Len:7 >>;
payload_length_to_binary(Len) when Len =< 16#ffff ->
  << 126:7, Len:16 >>;
payload_length_to_binary(Len) when Len =< 16#7fffffffffffffff ->
  << 127:7, Len:64 >>.

%% @doc Converts an atom to the appropriate opcode (as per spec).
-spec atom_to_opcode(atom()) -> opcode().
atom_to_opcode(continue) -> 0;
atom_to_opcode(text) -> 1;
atom_to_opcode(binary) -> 2;
atom_to_opcode(close) -> 8;
atom_to_opcode(ping) -> 9;
atom_to_opcode(pong) -> 10.

%% @doc Converts an opcode to the appropriate atom (as per spec).
-spec opcode_to_atom(opcode()) -> atom().
opcode_to_atom(0) -> continue;
opcode_to_atom(1) -> text;
opcode_to_atom(2) -> binary;
opcode_to_atom(8) -> close;
opcode_to_atom(9) -> ping;
opcode_to_atom(10) -> pong.
