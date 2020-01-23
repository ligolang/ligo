let check_signature (pk, signed, msg: key * signature * bytes) : bool =
  Crypto.check pk signed msg
