type storage = bytes

[@entry]
let main (_ignore : unit) (store : storage) : operation list * storage
=
  let packed = Bytes.pack (Tezos.get_chain_id ()) in
  if store <> packed then
    (failwith "wrong chain" : operation list * storage)
  else ([], packed)