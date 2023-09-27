module TacoShop = struct
  type taco_supply =
    {
     current_stock : nat;
     max_price : tez
    }

  type taco_shop_storage = (nat, taco_supply) map

  [@entry]
  let buy_taco (taco_kind_index : nat) (taco_shop_storage : taco_shop_storage)
  : operation list * taco_shop_storage =
    (* Retrieve the taco_kind from the contract's storage or fail *)

    let taco_kind =
      match Map.find_opt (taco_kind_index) taco_shop_storage with
        Some k -> k
      | None -> failwith "Unknown kind of taco" in
    let current_purchase_price : tez =
      taco_kind.max_price / taco_kind.current_stock in
    (* We won't sell tacos if the amount is not correct *)

    let () =
      if (Tezos.get_amount ()) <> current_purchase_price
      then
        failwith
          "Sorry, the taco you are trying to purchase has a different price" in
    (* Update the storage decreasing the stock by 1n *)

    let taco_shop_storage =
      Map.update
        taco_kind_index
        (Some
           {taco_kind with current_stock = abs (taco_kind.current_stock - 1n)})
        taco_shop_storage in
    [], taco_shop_storage

end

let default_storage : TacoShop.taco_shop_storage =
  Map.literal
    [
      (1n,
       {
        current_stock = 50n;
        max_price = 50000000mutez
       });
      (2n,
       {
        current_stock = 20n;
        max_price = 75000000mutez
       })
    ]
