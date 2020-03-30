type inner_storage = (int,nat) michelson_or
type storage = (int,inner_storage) michelson_or 

type return = operation list * storage

let main (action, store : unit * storage) : return =
  let foo = (M_right (M_left 1 : inner_storage) : storage) in
  (([] : operation list), (foo: storage))
