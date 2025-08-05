module RewardsWithFlaw = struct

  type storage = {
    owner : address;
    beneficiaries : address list
  }

  (* Send rewards to one address *)
  let send_one_reward (beneficiary_addr : address) : operation =
    let contract_opt =
      Tezos.get_contract_opt beneficiary_addr in
    let beneficiary =
      match contract_opt with
        Some contract -> contract
      | None -> (failwith "CONTRACT_NOT_FOUND" : unit contract) in
    Tezos.Operation.transaction () 5tez beneficiary

  (* Send rewards to all beneficiaries *)
  [@entry]
  let send_rewards (_ : unit) (storage : storage) : operation list * storage =
    if Tezos.get_sender () <> storage.owner
    then failwith "Not the owner"
    else let operations = List.map send_one_reward storage.beneficiaries in
    operations, storage

  [@entry]
  let change_owner (new_owner : address) (storage : storage) : operation list * storage =
    (* Verify that the sender is the admin *)
    let _ = if Tezos.get_sender () <> storage.owner then failwith "Not the owner" in
    [], { storage with owner = new_owner }

end