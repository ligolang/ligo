module TacoShop = struct
  type taco_supply =
    {
     current_stock : nat;
     max_price : tez
    }

  type taco_shop_storage = (nat, taco_supply) map

  [@entry]
  let buy_taco (taco_kind_index : nat) (taco_shop_storage : taco_shop_storage) : operation list * taco_shop_storage =
    [], taco_shop_storage

end 