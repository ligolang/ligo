open Test_helpers

let mfile_FA1  = "./contracts/FA1.mligo"

let compile_main ~raise f _s () =
  Test_helpers.compile_main ~raise f ()

open Ast_imperative


let (sender , contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let external_contract =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 4 in
  let kh = id.public_key_hash in
  Tezos_utils.Signature.Public_key_hash.to_string kh

let from_  = e_address @@ addr 5
let to_    = e_address @@ addr 2
let sender = e_address @@ sender
let external_contract = e_annotation (e_constant (Const C_IMPLICIT_ACCOUNT) [e_key_hash external_contract]) (t_contract (t_nat ()))

let transfer ~raise f s () =
  let program = get_program ~raise f ~st:s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_pair from_ (e_pair to_ @@ e_nat 10) in
  let new_storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 90); (to_, e_nat 110)]);
    ("total_supply",e_nat 300);
  ] in
  let input = e_pair parameter storage in
  let expected = e_pair (e_typed_list [] (t_operation ())) new_storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_eq ~raise program ~options "transfer" input expected

let transfer_not_e_balance ~raise f s () =
  let program = get_program ~raise f ~st:s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 0); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", from_); ("spender", sender)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("address_from", from_);("address_to",to_); ("value",e_nat 10)] in
  let input = e_pair parameter storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_string_failwith ~raise ~options program "transfer" input
  "NotEnoughBalance"

let get_balance ~raise f s () =
  let program = get_program ~raise f ~st:s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", sender); ("spender", from_)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("owner", from_);("callback", external_contract)] in
  let input = e_pair parameter storage in
  let expected = e_pair (e_typed_list [] (t_operation ())) storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_eq ~raise program ~options "getBalance" input expected

let get_total_supply ~raise f s () =
  let program = get_program ~raise f ~st:s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", sender); ("spender", from_)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("callback", external_contract)] in
  let input = e_pair parameter storage in
  let expected = e_pair (e_typed_list [] (t_operation ())) storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_eq ~raise program ~options "getTotalSupply" input expected

let main = test_suite "tzip-5" [
  test_w "compile"                           (compile_main             mfile_FA1 "cameligo");
  (* test_w "transfer"                          (transfer                 mfile_FA1 "cameligo");
  test_w "transfer (not enough balance)"     (transfer_not_e_balance   mfile_FA1 "cameligo"); can't test curried entrypoint yet *)
  (* test "getAllowance"                      (get_allowance            mfile_FA12 "cameligo");
  test "getBalance"                        (get_balance              mfile_FA12 "cameligo");
  test "getTotalSupply"                    (get_total_supply         mfile_FA12 "cameligo"); waiting for a dummy_contract with type nat contractt*)
  ]
