[@entry]
let main (_ : unit) (_ : unit -> address) : operation list * (unit -> address) =
  [], (fun (_ : unit) -> Tezos.get_self_address ())
