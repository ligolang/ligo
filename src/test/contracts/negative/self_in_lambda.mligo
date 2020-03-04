let foo (u : unit) : address = Tezos.self_address

let main (ps: unit * address): (operation list * address) =
  let dummy = foo () in (* force not to inline foo *)
  ( ([] : operation list) , foo ())
