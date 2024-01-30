#import "gitlab-pages/docs/tutorials/taco-shop/src/tezos-taco-shop-smart-contract/TacoShop.mligo" "TacoShop"

let assert_string_failure (res : test_exec_result) (expected : string) =
  let expected = Test.eval expected in
  match res with
  | Fail (Rejected (actual,_)) -> assert (Test.michelson_equal actual expected)
  | Fail _ -> failwith "contract failed for an unknown reason"
  | Success _ -> failwith "bad price check"

let test =
  (* originate the contract with a initial storage *)
  let init_storage = Map.literal [
      (1n, { current_stock = 50n ; max_price = 50tez }) ;
      (2n, { current_stock = 20n ; max_price = 75tez }) ; ]
  in
  let { addr ; code = _; size = _ } = Test.originate (contract_of TacoShop) init_storage 0tez in

  (* Test inputs *)
  let clasico_kind : TacoShop parameter_of = Buy_taco 1n in
  let unknown_kind : TacoShop parameter_of = Buy_taco 3n in

  (* Auxiliary function for testing equality in maps *)
  let eq_in_map (r : TacoShop.taco_supply) (m : TacoShop.taco_shop_storage) (k : nat) =
    match Map.find_opt k m with
    | None -> false
    | Some v -> v.current_stock = r.current_stock && v.max_price = r.max_price in

  (* Purchasing a Taco with 1tez and checking that the stock has been updated *)
  let ok_case : test_exec_result = Test.transfer addr clasico_kind 1tez in
  let () = match ok_case with
    | Success _ ->
      let storage = Test.get_storage addr in
      assert ((eq_in_map { current_stock = 49n ; max_price = 50tez } storage 1n) &&
              (eq_in_map { current_stock = 20n ; max_price = 75tez } storage 2n))
    | Fail _ -> failwith ("ok test case failed")
  in

  (* Purchasing an unregistred Taco *)
  let nok_unknown_kind = Test.transfer addr unknown_kind 1tez in
  let () = assert_string_failure nok_unknown_kind "Unknown kind of taco" in

  (* Attempting to Purchase a Taco with 2tez *)
  let nok_wrong_price = Test.transfer addr clasico_kind 2tez in
  let () = assert_string_failure nok_wrong_price "Sorry, the taco you are trying to purchase has a different price" in
  ()