let test_bytes_bitwise_ops  =
  let b_and         = 0x0005 land 0x0106 in
  let b_or          = 0x0005 lor  0x0106 in
  let b_xor         = 0x0005 lxor 0x0106 in
  let b_shift_left  = 0x06   lsl  8n     in
  let b_shift_right = 0x0006 lsr  1n     in

  assert (b_and         = 0x0004 &&
          b_or          = 0x0107 &&
          b_xor         = 0x0103 &&
          b_shift_left  = 0x0600 &&
          b_shift_right = 0x0003  )