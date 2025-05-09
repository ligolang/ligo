module Tezos = Tezos.Next

let test =
  let c : chain_id = Tezos.get_chain_id () in
  Bytes.pack c
