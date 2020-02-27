let foo (u: unit) : address =
  Current.self_address

let main (ps: unit * address): (operation list * (unit -> address)) =
  ( ([] : operation list) , foo)
