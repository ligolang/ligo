let check (p : key_hash) =
  let c = Tezos.implicit_account p
  in Tezos.address c