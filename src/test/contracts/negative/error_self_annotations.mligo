type param =
| [@annot foo] A of unit
| B of unit

[@entry]
let main (_ : param) (_ : unit) : operation list * unit =
  let c = (Tezos.self ("%a") : unit contract) in
  let op = Tezos.transaction () 0mutez c in
  ([op] : operation list), ()
