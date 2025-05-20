module Testviews = struct

  type storage = string

  type return_type = operation list * storage

  [@entry]
  let set (inputStr : storage) (_storage: storage) : return_type =
   [], inputStr

  [@entry]
  let reset (_u : unit) (_storage : storage) : return_type =
    [], ""

  [@view]
  let getString (_u : unit) (storage : storage) : string =
    storage
end
let test_view =
  let contract = Test.Originate.contract (contract_of Testviews) "" 0tez in
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "set" contract.taddr) "hello" 0tez in
  let address = Test.Typed_address.to_address contract.taddr in
  let viewResultOption : string option = Tezos.View.call "getString" unit address in
  let viewResult = match viewResultOption with
    | Some str -> str
    | None -> ""
  in
  Assert.assert (Test.Compare.eq viewResult "hello")