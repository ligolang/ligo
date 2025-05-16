module Test = Test.Next

module TacoShop = struct
  type taco_supply = { current_stock: nat; max_price: tez }
  type taco_data = (nat, taco_supply) map
  type admin_address = address
  type storage = {
    admin_address: admin_address;
    taco_data: taco_data;
  }

  let default_taco_data: taco_data = Map.literal [
    (1n, { current_stock = 50n; max_price = 50tez });
    (2n, { current_stock = 20n; max_price = 75tez });
  ]

  (* Internal function to get the price of a taco *)
  let get_taco_price_internal (taco_kind_index : nat) (taco_data : taco_data) : tez =
    let taco_kind : taco_supply =
      match Map.find_opt taco_kind_index taco_data with
      | Some kind -> kind
      | None -> failwith "Unknown kind of taco"
      in
      taco_kind.max_price / taco_kind.current_stock

  [@view]
  let get_taco_price (taco_kind_index : nat) (storage : storage) : tez =
    get_taco_price_internal taco_kind_index storage.taco_data

  (* Buy a taco *)
  [@entry]
  let buy_taco (taco_kind_index : nat) (storage : storage) : operation list * storage =

    let { admin_address; taco_data } = storage in

    (* Retrieve the kind of taco from the contracts storage or fail *)
    let taco_kind : taco_supply =
      match Map.find_opt taco_kind_index taco_data with
      | Some kind -> kind
      | None -> failwith "Unknown kind of taco" in

    (* Get the current price of this type of taco *)
    let current_purchase_price = get_taco_price_internal taco_kind_index taco_data in

    (* Verify that the caller sent the correct amount of tez *)
    let _ = if (Tezos.get_amount () <> current_purchase_price) then
      failwith "Sorry, the taco you are trying to purchase has a different price" in

    (* Verify that there is at least one of this type of taco *)
    let _ = if (taco_kind.current_stock = 0n) then
      failwith "Sorry, we are out of this type of taco" in

    (* Update the storage with the new quantity of tacos *)
    let updated_taco_data : taco_data = Map.update
      taco_kind_index
      (Some { taco_kind with current_stock = abs (taco_kind.current_stock - 1n) })
      taco_data in

    let updated_storage : storage = {
      admin_address = admin_address;
      taco_data = updated_taco_data;
    } in

    [], updated_storage

  [@entry]
  let payout (_u : unit) (storage : storage) : operation list * storage =

    (* Ensure that only the admin can call this entrypoint *)
    let _ = if (Tezos.get_sender () <> storage.admin_address) then
      failwith "Only the admin can call this entrypoint" in

    (* Create contract object that represents the target account *)
    let receiver_contract = match Tezos.get_contract_opt storage.admin_address with
    | Some contract -> contract
    | None -> failwith "Couldn't find account" in

    (* Create operation to send tez *)
    let payout_operation = Tezos.Operation.transaction unit (Tezos.get_balance ()) receiver_contract in

    (* Restore stock of tacos *)
    let new_storage : storage = {
      admin_address = storage.admin_address;
      taco_data = default_taco_data
    } in

    [payout_operation], new_storage

end

(* Convenience function to get current taco price *)
let get_taco_price (untyped_address : address) (taco_kind_index : nat) : tez =
  let view_result_option : tez option = Tezos.View.call
    "get_taco_price"
    taco_kind_index
    untyped_address in
  match view_result_option with
  | Some cost_mutez -> cost_mutez
  | None -> Test.Assert.failwith "Couldn't get the price of a taco"

(* Convenience function for testing equality in maps *)
let eq_in_map (r : TacoShop.taco_supply) (m : TacoShop.taco_data) (k : nat) =
  match Map.find_opt k m with
  | None -> false
  | Some v -> v.current_stock = r.current_stock && v.max_price = r.max_price

let test =

  (* Set the initial storage and deploy the contract *)
  let admin_address : address = Test.Account.address 0n in
  let initial_storage : TacoShop.storage = {
    admin_address = admin_address;
    taco_data = TacoShop.default_taco_data
  } in
  let contract = Test.Originate.contract (contract_of TacoShop) initial_storage 0tez in

  (* Get the current price of a taco *)
  let untyped_address = Test.Typed_address.to_address contract.taddr in
  let current_price = get_taco_price untyped_address 1n in

  (* Purchase a taco *)
  let success_result =
    Test.Contract.transfer
      (Test.Typed_address.get_entrypoint "buy_taco" contract.taddr)
      1n
      current_price
    in

  (* Verify that the stock was updated *)
  let () = match success_result with
    | Success _s ->
      let storage = Test.Typed_address.get_storage contract.taddr in
      let () = Assert.assert (eq_in_map
        { current_stock = 49n; max_price = 50000000mutez }
        storage.taco_data
        1n
      ) in
      let () = Assert.assert (eq_in_map
        { current_stock = 20n; max_price = 75000000mutez }
        storage.taco_data
        2n
      ) in
      Test.IO.log "Successfully bought a taco"
    | Fail err -> failwith err
    in

  (* Fail to purchase a taco without sending enough tez *)
  let fail_result = Test.Contract.transfer
    (Test.Typed_address.get_entrypoint "buy_taco" contract.taddr)
    1n
    1mutez in
  let () = match fail_result with
  | Success _s -> failwith "Test was able to buy a taco for the wrong price"
  | Fail _err -> Test.IO.log "Contract successfully blocked purchase with incorrect price" in

  (* Test the payout entrypoint as the administrator *)
  let admin_balance_before = Test.Address.get_balance admin_address in

  let () = Test.State.set_source admin_address in

  let payout_result = Test.Contract.transfer
    (Test.Typed_address.get_entrypoint "payout" contract.taddr)
    unit
    0tez
  in
  let () = match payout_result with
  | Success _s -> let storage = Test.Typed_address.get_storage contract.taddr in
    let () = Assert.assert
      (eq_in_map (Map.find 1n TacoShop.default_taco_data)
      storage.taco_data
      1n) in
    let () = Assert.assert
      (eq_in_map (Map.find 2n TacoShop.default_taco_data)
      storage.taco_data
      2n) in
    Test.IO.log "Successfully reset taco storage"
  | Fail _err -> failwith "Failed to reset taco storage" in

  (* Check that the admin account got a payout *)
  let admin_balance_after = Test.Address.get_balance admin_address in
  let () = Assert.assert (Test.Compare.lt admin_balance_before admin_balance_after) in

  (* Verify that the entrypoint fails if called by someone else *)
  let other_user_account = Test.Account.address 1n in
  let _ = Test.State.set_source other_user_account in
  let failed_payout_result = Test.Contract.transfer
    (Test.Typed_address.get_entrypoint "payout" contract.taddr)
    unit
    0tez
  in
  match failed_payout_result with
  | Success _s -> failwith "A non-admin user was able to call the payout entrypoint"
  | Fail _err -> Test.IO.log "Successfully prevented a non-admin user from calling the payout entrypoint"