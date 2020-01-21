let foo (u: unit) : address =
  Current.self_address

let main (ps: unit * address): (operation list * address) =
  ( ([] : operation list) , foo)