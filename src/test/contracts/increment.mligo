type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints

let add (store : storage) (delta : int) : storage = store + delta
let sub (store : storage) (delta : int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
   
let main (action : parameter) (store : storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add store n
 | Decrement (n) -> sub store n
 | Reset         -> 0)
