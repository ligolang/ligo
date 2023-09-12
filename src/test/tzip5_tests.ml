open Test_helpers

let mfile_FA1 = "./contracts/FA1.mligo"
let compile_main ~raise f _s () = Test_helpers.compile_main ~raise f ()

open Ast_unified

let sender =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (Lwt_main.run @@ test_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt


let from_ = e_address ~loc @@ Lwt_main.run @@ addr 5
let to_ = e_address ~loc @@ Lwt_main.run @@ addr 2
let sender = e_address ~loc @@ sender

let transfer ~raise f s () =
  let program = get_program ~raise f ~st:s () in
  let storage =
    e_record_ez
      ~loc
      [ ( "ledger"
        , e_big_map
            ~loc
            [ sender, e_nat ~loc 100; from_, e_nat ~loc 100; to_, e_nat ~loc 100 ] )
      ; "totalSupply", e_nat ~loc 300
      ]
  in
  let parameter = e_pair ~loc from_ (e_pair ~loc to_ @@ e_nat ~loc 10) in
  let new_storage =
    e_record_ez
      ~loc
      [ ( "ledger"
        , e_big_map
            ~loc
            [ sender, e_nat ~loc 100; from_, e_nat ~loc 90; to_, e_nat ~loc 110 ] )
      ; "totalSupply", e_nat ~loc 300
      ]
  in
  let expected = e_pair ~loc (e_list ~loc []) new_storage in
  let options = make_options () in
  expect_eq_twice ~raise program ~options "transfer" parameter storage expected


let transfer_not_e_balance ~raise f s () =
  let program = get_program ~raise f ~st:s () in
  let storage =
    e_record_ez
      ~loc
      [ "totalSupply", e_nat ~loc 300
      ; ( "ledger"
        , e_big_map
            ~loc
            [ sender, e_nat ~loc 100; from_, e_nat ~loc 0; to_, e_nat ~loc 100 ] )
      ]
  in
  let parameter = e_pair ~loc from_ (e_pair ~loc to_ (e_nat ~loc 10)) in
  let options = make_options () in
  expect_string_failwith_twice
    ~raise
    ~options
    program
    "transfer"
    parameter
    storage
    "NotEnoughBalance"


let main =
  test_suite
    "tzip-5"
    [ test_w "compile" (compile_main mfile_FA1 "cameligo")
    ; test_w "transfer_no_balance" (transfer_not_e_balance mfile_FA1 "transfer")
    ; test_w "transfer" (transfer mfile_FA1 "transfer")
    ]
