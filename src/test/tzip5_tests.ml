open Test_helpers

let mfile_FA1 = "./contracts/FA1.mligo"
let compile_main ~raise f _s () = Test_helpers.compile_main ~raise f ()

open Ast_unified

let sender, contract =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt, kt


let external_contract =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 4 in
  let kh = id.public_key_hash in
  Tezos_utils.Signature.Public_key_hash.to_string kh


let from_ = e_address ~loc @@ addr 5
let to_ = e_address ~loc @@ addr 2
let sender = e_address ~loc @@ sender

let transfer ~raise f s () =
  let program = get_program ~raise f ~st:s () in
  let storage =
    e_record_ez
      ~loc
      [ ( "tokens"
        , e_big_map
            ~loc
            [ sender, e_nat ~loc 100; from_, e_nat ~loc 100; to_, e_nat ~loc 100 ] )
      ; "total_supply", e_nat ~loc 300
      ]
  in
  let parameter = e_pair ~loc from_ (e_pair ~loc to_ @@ e_nat ~loc 10) in
  let new_storage =
    e_record_ez
      ~loc
      [ ( "tokens"
        , e_big_map
            ~loc
            [ sender, e_nat ~loc 100; from_, e_nat ~loc 90; to_, e_nat ~loc 110 ] )
      ; "total_supply", e_nat ~loc 300
      ]
  in
  let input = e_pair ~loc parameter storage in
  let expected = e_pair ~loc (e_list ~loc []) new_storage in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ())
  in
  expect_eq ~raise program ~options "transfer" input expected


let transfer_not_e_balance ~raise f s () =
  let program = get_program ~raise f ~st:s () in
  let storage =
    e_record_ez
      ~loc
      [ ( "tokens"
        , e_big_map
            ~loc
            [ sender, e_nat ~loc 100; from_, e_nat ~loc 0; to_, e_nat ~loc 100 ] )
      ; ( "allowances"
        , e_big_map
            ~loc
            [ e_record_ez ~loc [ "owner", from_; "spender", sender ], e_nat ~loc 100 ] )
      ; "total_supply", e_nat ~loc 300
      ]
  in
  let parameter =
    e_record_ez ~loc [ "address_from", from_; "address_to", to_; "value", e_nat ~loc 10 ]
  in
  let input = e_pair ~loc parameter storage in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ())
  in
  expect_string_failwith ~raise ~options program "transfer" input "NotEnoughBalance"


let main = test_suite "tzip-5" [ test_w "compile" (compile_main mfile_FA1 "cameligo") ]
