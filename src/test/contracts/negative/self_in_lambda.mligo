let foo (u: unit) : address =
  Current.self_address

let main (ps: unit * address): (operation list * address) =
  let dummy = foo() in
  ( ([] : operation list) , foo())
