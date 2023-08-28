let foo (_u : unit) : address =
  Tezos.address (Tezos.self "%default" : unit contract)

[@entry]
let main () (_ : address) : (operation list * address) =
  let _dummy = foo () in
  (* force not to inline foo *)
  (([] : operation list), foo ())
