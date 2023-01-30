
type storage = int
type return = operation list * storage

let main (p : unit) (s : storage) : return =
  let x : nat = 1 + 2 + 3 (* int *) in
  [],s
