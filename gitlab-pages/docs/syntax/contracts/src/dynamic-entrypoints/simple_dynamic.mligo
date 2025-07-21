module DynamicContract = struct
  type internal_storage = int
  type storage_type = {
    storage : internal_storage;
    dynamic_entrypoints : Dynamic_entrypoints.t; // big_map<nat, bytes>
  }
  type return_type = operation list * storage_type
  type dyn_return_type = operation list * internal_storage

  // Dynamic entrypoint: double the integer in storage
  [@dyn_entry]
  let double (() : unit) (s : internal_storage) : dyn_return_type = [], s + s

  // Dynamic entrypoint: square the integer in storage
  [@dyn_entry]
  let square (() : unit) (s : internal_storage) : dyn_return_type = [], s * s

  // Initially, this dynamic entrypoint does nothing
  // But the changeAction entrypoint sets it to a different dynamic entrypoint
  [@dyn_entry]
  let currentAction (() : unit) (s : internal_storage) : dyn_return_type = [], s

  // Run the currentAction entrypoint
  [@entry]
  let runAction (() : unit) (full_storage : storage_type) : return_type =
    let {storage; dynamic_entrypoints} = full_storage in

    match (Dynamic_entrypoints.get currentAction dynamic_entrypoints) with
      Some f ->
        let (operations, newStorage) = f unit storage in
        operations, {
          storage = newStorage;
          dynamic_entrypoints = dynamic_entrypoints
        }
      | None -> failwith "Error"

  // Change the currentAction entrypoint to double or square
  [@entry]
  let changeAction (new_action_str : string) (full_storage : storage_type) : return_type =
    let {storage; dynamic_entrypoints} = full_storage in

    let new_dynamic_entrypoints : Dynamic_entrypoints.t =
      if (new_action_str = "double")
      then let new_action = match (Dynamic_entrypoints.get double dynamic_entrypoints) with
        | Some f -> f
        | None -> failwith "Error" in
        Dynamic_entrypoints.set currentAction (Some new_action) dynamic_entrypoints
      else let new_action = match (Dynamic_entrypoints.get square dynamic_entrypoints) with
        | Some f -> f
        | None -> failwith "Error" in
        Dynamic_entrypoints.set currentAction (Some new_action) dynamic_entrypoints in

    [], {
      storage = storage;
      dynamic_entrypoints = new_dynamic_entrypoints
    }

end
let test_dyn =
  // Generate storage with dynamic entrypoints
  let initial_storage = Test.Dynamic_entrypoints.storage (contract_of DynamicContract) 3 in
  let contract = Test.Originate.contract (contract_of DynamicContract) initial_storage 0mutez in
  let storage_before = Test.Typed_address.get_storage contract.taddr in
  let () = Assert.assert (Test.Compare.eq storage_before.storage 3) in

  // At the start, the runAction dynamic entrypoint does nothing
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "runAction" contract.taddr) unit 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  // Verify that storage did not change
  let () = Assert.assert (Test.Compare.eq storage storage_before) in

  // Set current action to double
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "changeAction" contract.taddr) "double" 0tez in

  // Double storage to 6
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "runAction" contract.taddr) unit 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  let () = Assert.assert (Test.Compare.eq storage.storage 6) in

  // Double storage to 12
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "runAction" contract.taddr) unit 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  let () = Assert.assert (Test.Compare.eq storage.storage 12) in

  // Switch to square
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "changeAction" contract.taddr) "square" 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  let () = Assert.assert (Test.Compare.eq storage.storage 12) in

  // Square storage to 144
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "runAction" contract.taddr) unit 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  Assert.assert(Test.Compare.eq storage.storage 144)