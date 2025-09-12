let id_string (p : string) : string option =
  let packed: bytes = Bytes.pack p in
  Bytes.unpack packed