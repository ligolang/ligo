type parameter =
  Increment of int
| Decrement of int

type storage = int

type return = operation list * storage

let add (n, store : int * storage) : storage = store + n
let sub (n, store : int * storage) : storage = store - n

let main (action, store : parameter * storage) : return =
  [],
  match action with
    Increment n -> add (n, store)
  | Decrement n -> sub (n, store)
