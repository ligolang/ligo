type storage = int

// Two entrypoints

let add (store : storage) (delta : int) = store + delta

let sub (store : storage) (delta : int) = store - delta

module Foo = struct

  [@entry]
  let reset () (_ : storage) : operation list * storage = [], 0

  [@entry]
  let decrement (n : int) (store : storage) : operation list * storage = [], sub store n

  [@entry]
  let increment (n : int) (store : storage) : operation list * storage = [], add store n

  [@view]
  let foo (i : int) (store : storage) : int = i + store

end

[@view]
let bor (i : int) (store : storage) : int = 1 + i + store

module X = Foo

module Bar = struct
  module Foo = X

  module C = Foo

end

module C = Bar.C

(* Tests for main access point *)

let initial_storage = 42

let test_initial_storage =
  let orig = Test.originate (contract_of C) initial_storage 0mutez in
  assert (Test.get_storage orig.addr = initial_storage)

let test_increment =
  let orig = Test.originate (contract_of C) initial_storage 0mutez in
  let contr = Test.to_contract orig.addr in
  let _ = Test.transfer_to_contract_exn contr (Increment 1) 1mutez in
  assert (Test.get_storage orig.addr = initial_storage + 1)
