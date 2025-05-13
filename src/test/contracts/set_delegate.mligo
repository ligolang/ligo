let check (p : key_hash) : operation list =
  let _useless : operation = Tezos.Operation.set_delegate (Some p)
  in []
