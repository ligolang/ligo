let main (p : key_hash) : operation list =
  let useless : operation = Tezos.set_delegate (Some p)
  in ([] : operation list)
