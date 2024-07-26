let check (kh : key_hash) : operation list =
  [Tezos.set_delegate (Some kh)]