let check (p,s : unit * tez) = [], Tezos.get_balance()
let threshold (p : unit) = if Tezos.get_amount () = 100tz then 42 else 0
let check (p : unit) = Tezos.get_sender ()
let check (p : key_hash) =
  let c = Tezos.implicit_account p
  in Tezos.address c
let check (p : unit) = Tezos.get_self_address ()
let check (p : unit) = Tezos.self("%default")
let check (kh : key_hash) = Tezos.implicit_account kh
let check (p : unit) = Tezos.get_source ()
type storage = bytes

[@entry]
let main (_ignore : unit) (store : storage) =
  let packed = Bytes.pack (Tezos.get_chain_id ()) in
  if (store <> packed) then
    (failwith "wrong chain" : (operation list * storage))
  else
    ([], (packed: storage))