let main (p, s : unit * (unit -> address)) : operation list * (unit -> address) =
  (([] : operation list), (fun (u : unit) -> Tezos.self_address))
