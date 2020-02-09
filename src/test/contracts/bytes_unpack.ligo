function id_string (const p : string) : option(string) is block {
  const packed : bytes = bytes_pack(p) ;
} with (bytes_unpack(packed): option(string))

function id_int (const p : int) : option(int) is block {
  const packed : bytes = bytes_pack(p) ;
} with (bytes_unpack(packed): option(int))

function id_address (const p : address) : option(address) is block {
  const packed : bytes = bytes_pack(p) ;
} with (bytes_unpack(packed): option(address))
