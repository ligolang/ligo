module Test = Test.Next

module OneEntrypoint = struct
  type storage = int
  type return_type = operation list * storage

  [@entry]
  let increment (_ : unit) (storage : storage) : return_type =
    [], storage + 1

end

let test_one_entrypoint =
  let initial_storage = 42 in
  let contract = Test.Originate.contract (contract_of OneEntrypoint) initial_storage 0tez in
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "default" contract.taddr) unit 0tez in
  Assert.assert ((Test.Typed_address.get_storage contract.taddr) = initial_storage + 1)