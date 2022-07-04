function id_string (const p : string) : option (string) is {
  const packed : bytes = Bytes.pack (p)
} with (Bytes.unpack (packed) : option (string))

function id_int (const p : int) : option (int) is {
  const packed : bytes = Bytes.pack (p)
} with (Bytes.unpack (packed) : option (int))

function id_address (const p : address) : option (address) is {
  const packed : bytes = Bytes.pack (p)
} with (Bytes.unpack (packed) : option (address))
