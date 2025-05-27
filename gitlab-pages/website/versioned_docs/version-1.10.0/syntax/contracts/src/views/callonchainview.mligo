module ContractWithView = struct
  type storage = int
  type return_type = operation list * storage

  [@entry] let main (param : int) (_storage : storage) : return_type =
    [], param

  [@view] let multiply (param : int) (storage : storage) : int =
    param * storage

end
module CallView = struct
  type storage = address * int
  type return_type = operation list * storage

  [@entry] let callView (param : int) (storage : storage) : return_type =
    let (targetAddress, _s) = storage in
    let resultOpt : int option = Tezos.call_view "multiply" param targetAddress in
    match resultOpt with
      Some newValue -> [], (targetAddress, newValue)
    | None -> failwith("Something went wrong")
end
let test =

  // Originate ContractWithView
  let contract1 = Test.Next.Originate.contract (contract_of ContractWithView) 5 0tez in
  let addr1 = Test.Next.Typed_address.to_address contract1.taddr in

  // Originate CallView with address of ContractWithView in storage
  let initial_storage = (addr1, 0) in
  let contract2 = Test.Next.Originate.contract (contract_of CallView) initial_storage 0tez in

  // Call callView
  let _ : nat = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint "default" contract2.taddr) 12 0tez in
  let (_address, integer) = Test.Next.Typed_address.get_storage contract2.taddr in
  Assert.assert(integer = 60)