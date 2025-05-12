module Test = Test.Next
module Tezos = Tezos.Next

module C = struct
  type storage = int
  type parameter = unit
  type return = operation list * storage

  let f (x : int) = x * 3 + 2

  let ct = Test.State.register_constant (Test.Michelson.eval f)

  [@entry]
  let main (() : parameter) (store : storage) : return =
    [], Tezos.constant ct store
end

let test =
  let orig = Test.Originate.contract (contract_of C) 1 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main ()) 0tez in
  Assert.assert (Test.get_storage orig.taddr = 5)