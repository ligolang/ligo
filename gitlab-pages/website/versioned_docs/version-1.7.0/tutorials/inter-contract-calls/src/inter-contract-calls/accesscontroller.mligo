(* examples/contracts/mligo/AccessController.mligo *)

type storage = {senders_whitelist : address set}

[@entry]
let call (op : unit -> operation) (s : storage) : operation list * storage =
  if Set.mem (Tezos.get_sender ()) s.senders_whitelist
  then [op ()], s
  else failwith "Sender is not whitelisted"

[@entry]
let iswhitelisted (arg : address * (bool contract)) (s : storage) : operation list * storage =
  let addr, callback_contract = arg in
  let whitelisted = Set.mem addr s.senders_whitelist in
  let op = Tezos.transaction whitelisted 0mutez callback_contract in
  [op], s