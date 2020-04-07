type storage = (int,string) michelson_or 
type foobar = (int, int ) michelson_or

type return = operation list * storage

let main (action, store : unit * storage) : return =
  let foo = (M_right ("one") : storage) in
  let bar = (M_right 1 : foobar) in
  (([] : operation list), (foo: storage))
