module Test = Test.Next

module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  let add (value : int) ( store: storage_type) : return_type =
    [], store + value

  [@entry]
  let sub (value : int) ( store: storage_type) : return_type =
    [], store - value

end

let test =
  let contract = Test.Originate.contract (contract_of Counter) 0 0tez in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "add" contract.taddr) 5 0tez in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "sub" contract.taddr) 2 0tez in
  Assert.assert ((Test.Typed_address.get_storage contract.taddr) = 3)