[@entry]
let changeDelegate (new_delegate : key_hash) (storage : unit) : operation list * unit =
  [Tezos.Operation.set_delegate (Some new_delegate)], storage