let assert = Assert.assert

module C = struct
  type storage = int
  type parameter = unit
  type return = operation list * storage

  let ct : michelson_program =
    Test.Next.Michelson.parse "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
  let ct : string = Test.Next.State.register_constant ct
  [@entry]
  let main (() : parameter)  (store : storage) : return =
  ([] : operation list), ((Tezos.constant ct : int -> int) store)
end

let test =
  let orig = Test.Next.Originate.contract (contract_of C) 1 0tez in
  let _ = Test.Next.Typed_address.transfer_exn orig.taddr (Main ()) 0tez in
  assert (Test.Next.Typed_address.get_storage orig.taddr = 5)
