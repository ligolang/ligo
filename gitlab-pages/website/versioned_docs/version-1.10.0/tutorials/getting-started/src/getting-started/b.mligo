module Counter = struct
  type storage = int
  type return_type = operation list * storage

  [@entry] let add (n : int) (storage : storage) : return_type = [], storage + n
  [@entry] let sub (n : int) (storage : storage) : return_type = [], storage - n
end

let test_add =
  let initial_storage = 10 in
  let orig = Test.Next.Originate.contract (contract_of Counter) initial_storage 0tez in
  let _ = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint "add" orig.taddr) 32 0tez in
  Assert.assert (Test.Next.Typed_address.get_storage(orig.taddr) = initial_storage + 32)