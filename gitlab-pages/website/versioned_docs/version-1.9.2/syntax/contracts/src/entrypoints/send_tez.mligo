type storage = unit
type return_value = operation list * storage

[@entry] let give5tez (_ : unit) (storage : storage) : return_value =
  if Tezos.get_balance () >= 5tez then
    let receiver_contract = match Tezos.get_contract_opt (Tezos.get_sender ()) with
      Some contract -> contract
    | None -> failwith "Couldn't find account" in
    let operation = Tezos.Next.Operation.transaction unit 5tez receiver_contract in
    [operation], storage
  else
    [], storage