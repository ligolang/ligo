type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

// Two entrypoints

let add (store : storage) (delta : int) = store + delta
let sub (store : storage) (delta : int) = store - delta

module Foo = struct
  let main (action : parameter) (store : storage) : operation list * storage =
   [],    // No operations
   (match action with
     Increment (n) -> add store n
   | Decrement (n) -> sub store n
   | Reset         -> 0)
  [@view] let foo (i : int) (store : storage) : int = i + store
  let bar (i : int) (store : storage) : int = 1 + i + store
end

[@view] let bor (i : int) (store : storage) : int = 1 + i + store

module X = Foo

module Bar = struct
  module Foo = X
  module C = Foo
end

module C = Bar.C

let kkmain = C.main

(* Tests for main access point *)

let initial_storage = 42

let test_initial_storage =
 let (taddr, _, _) = Test.originate C.main initial_storage 0tez in
 assert (Test.get_storage taddr = initial_storage)

let test_increment =
 let (taddr, _, _) = Test.originate C.main initial_storage 0tez in
 let contr = Test.to_contract taddr in
 let _ = Test.transfer_to_contract_exn contr (Increment 1) 1mutez in
 assert (Test.get_storage taddr = initial_storage + 1)
