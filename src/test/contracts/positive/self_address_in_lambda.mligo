[@entry]
let main (_ : unit) (_ : unit -> address) : operation list * (unit -> address) =
  (([] : operation list), (fun (_ : unit) -> Tezos.get_self_address ()))
