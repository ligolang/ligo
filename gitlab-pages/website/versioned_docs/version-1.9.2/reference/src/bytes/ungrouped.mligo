let concat_op (s : bytes) : bytes = Bytes.concat s 0x7070
let slice_op (s : bytes) = Bytes.sub 1n 2n s
let id_string (p : string) =
  let packed : bytes = Bytes.pack p in
  Bytes.unpack packed
let id_string (p : string) =
  let packed : bytes = Bytes.pack p in
  Bytes.unpack packed