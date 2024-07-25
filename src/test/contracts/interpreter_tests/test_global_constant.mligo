let assert = Assert.assert

module C = struct
  type storage = int
  type parameter = unit
  type return = operation list * storage

  let f (x : int) = x * 3 + 2

  let ct : string =
    Test.Next.State.register_constant (Test.Next.Michelson.eval f)

  [@entry]
  let main (() : parameter) (store : storage) : return =
    ([] : operation list), ((Tezos.constant ct : int -> int) store)
end

let test =
  let orig = Test.Next.Originate.contract (contract_of C) 1 0tez in
  let _ = Test.Next.Typed_address.transfer_exn orig.taddr (Main ()) 0tez in
  assert (Test.Next.Typed_address.get_storage orig.taddr = 5)
