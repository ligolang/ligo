let foo (u: unit) : address =
  Tezos.self_address

let main (ps: unit * address): (operation list * address) =
  ( ([] : operation list) , foo)
