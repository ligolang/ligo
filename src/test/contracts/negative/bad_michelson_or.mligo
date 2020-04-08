type storage = (int,string) michelson_or 

type return = operation list * storage

let main (action, store : unit * storage) : return =
  let foo = M_right ("one") in
  (([] : operation list), (foo: storage))
