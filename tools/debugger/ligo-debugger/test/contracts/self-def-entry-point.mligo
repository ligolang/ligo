[@entry]
let main (_,_ : unit * unit) : operation list * unit =
  let c = (Tezos.self("%default") : unit contract) in
  let op = Tezos.transaction () 0mutez c in
  (([op] : operation list), ())
