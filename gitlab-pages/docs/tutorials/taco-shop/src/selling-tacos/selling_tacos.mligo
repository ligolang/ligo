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

      (* Entrypoint logic goes here *)

      [], storage

  end