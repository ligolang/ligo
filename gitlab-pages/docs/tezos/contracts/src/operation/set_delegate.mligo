[@entry]
let changeDelegate (new_delegate : key_hash) (storage : unit) : operation list * unit =
  [Tezos.Next.Operation.set_delegate (Some new_delegate)], storage