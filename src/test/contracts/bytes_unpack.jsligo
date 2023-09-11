function id_string (p: string) : option<string> {
  let packed: bytes = Bytes.pack (p)
  return ((Bytes.unpack (packed)) as option <string>)
}

function id_int (p: int) : option<int> {
  let packed: bytes = Bytes.pack (p)
  return ((Bytes.unpack (packed)) as option <int>)
}

function id_address (p: address) : option<address> {
  let packed: bytes = Bytes.pack (p)
  return ((Bytes.unpack (packed)) as option <address>)
}
