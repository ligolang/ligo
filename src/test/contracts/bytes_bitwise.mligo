[@entry] let main (_ : unit) (_ : bytes) : operation list * bytes  =
  let b_and         = 0x0005 land 0x0106 in
  let b_or          = 0x0005 lor  0x0106 in
  let b_xor         = 0x0005 lxor 0x0106 in
  let b_shift_left  = 0x06   lsl  8n     in
  let b_shift_right = 0x0006 lsr  1n     in
  [], Bytes.concats [b_and ; b_or ; b_xor ; b_shift_left ; b_shift_right]
