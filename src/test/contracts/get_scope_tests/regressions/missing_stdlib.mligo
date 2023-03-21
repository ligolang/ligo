let main (p : key_hash) : address =
  let c : unit contract = Tezos.implicit_account (p) in
  Tezos.address c
