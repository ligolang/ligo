let test =
  let c : chain_id = Tezos.get_chain_id () in
  Bytes.pack c