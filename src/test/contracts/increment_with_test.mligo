module Test = Test.Next

type storage = int

type parameter =
| Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints

let add (store : storage) (delta : int) : storage = store + delta

let sub (store : storage) (delta : int) : storage = store - delta

let test_bar = Test.IO.log "tururu"

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)

[@entry]
let main (action : parameter) (store : storage) : return =
  [],
  (match action with
     Increment (n) -> add store n
   | Decrement (n) -> sub store n
   | Reset -> 0)

let y = 32

let z = Increment y

let test_foo = Test.IO.log "arrorro"

let z = (y, z)
