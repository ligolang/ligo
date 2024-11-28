type storage = unit
type return_value = operation list * storage

[@entry] let callContract (param : (address * string)) (storage : storage): return_value =
  let (addr, parameter) = param in
  let receiver_contract = match Tezos.get_contract_opt(addr) with
    Some contract -> contract
  | None -> failwith "Couldn't find contract" in
  let operations = [Tezos.Next.Operation.transaction parameter 0tez receiver_contract] in
  operations, storage