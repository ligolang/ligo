open Simple_utils.Trace
open Test_helpers
open Main_errors
open Ast_imperative.Combinators

let init_env = Environment.default Environment.Protocols.current
let type_file f = type_file f options

(*
let type_alias ~raise () : unit =
  let program = type_file ~raise "./contracts/type-alias.ligo" in
  expect_eq_evaluate ~raise program "foo" (e_int ~loc 23)

let function_ ~raise () : unit =
  let program = type_file ~raise "./contracts/function.ligo" in
  let make_expect n = n in
  expect_eq_n_int ~raise program "main" make_expect

let blockless ~raise () : unit =
  let program = type_file ~raise "./contracts/blockless.ligo" in
  let make_expect n = n + 10 in
  expect_eq_n_int ~raise program "blockless" make_expect

let assign ~raise () : unit =
  let program = type_file ~raise "./contracts/assign.ligo" in
  let make_expect n = n + 1 in
  expect_eq_n_int ~raise program "main" make_expect

let annotation ~raise () : unit =
  let program = type_file ~raise "./contracts/annotation.ligo" in
  let () = expect_eq_evaluate ~raise program "lst" (e_list ~loc []) in
  let () =
    expect_eq_evaluate
      ~raise
      program
      "my_address"
      (e_address ~loc "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
  in
  ()

let complex_function ~raise () : unit =
  let program = type_file ~raise "./contracts/function-complex.ligo" in
  let make_expect n = (3 * n) + 2 in
  expect_eq_n_int ~raise program "main" make_expect

let anon_function ~raise () : unit =
  let program = type_file ~raise "./contracts/function-anon.ligo" in
  let () = expect_eq_evaluate ~raise program "x" (e_int ~loc 42) in
  ()

let application ~raise () : unit =
  let program = type_file ~raise "./contracts/application.ligo" in
  let () =
    let expected = e_int ~loc 42 in
    expect_eq_evaluate ~raise program "x" expected
  in
  let () =
    let expected = e_int ~loc 42 in
    expect_eq_evaluate ~raise program "y" expected
  in
  let () =
    let expected = e_int ~loc 42 in
    expect_eq_evaluate ~raise program "z" expected
  in
  ()
*)

let variant ~raise f : unit =
  let program = type_file ~raise f in
  let () =
    let expected = e_constructor ~loc "Foo" (e_int ~loc 42) in
    expect_eq_evaluate ~raise program "foo" expected
  in
  let () =
    let expected = e_constructor ~loc "Bar" (e_bool ~loc true) in
    expect_eq_evaluate ~raise program "bar" expected
  in
  let () =
    let expected = e_constructor ~loc "Kee" (e_nat ~loc 23) in
    expect_eq_evaluate ~raise program "kee" expected
  in
  ()


(*
let variant_matching ~raise () : unit =
  let program = type_file ~raise "./contracts/variant-matching.ligo" in
  let () =
    let make_input n = e_constructor ~loc "Foo" (e_int ~loc n) in
    let make_expected = e_int ~loc in
    expect_eq ~raise program "fb" (make_input 0) (make_expected 0);
    expect_eq_n ~raise program "fb" make_input make_expected;
    expect_eq
      ~raise
      program
      "fb"
      (e_constructor ~loc "Kee" (e_nat ~loc 50))
      (e_int ~loc 23);
    expect_eq
      ~raise
      program
      "fb"
      (e_constructor ~loc "Bar" (e_bool ~loc true))
      (e_int ~loc 42);
    ()
  in
  ()

let closure ~raise () : unit =
  let program = type_file ~raise "./contracts/closure.ligo" in
  let program_1 = type_file ~raise "./contracts/closure-1.ligo" in
  let program_2 = type_file ~raise "./contracts/closure-2.ligo" in
  let program_3 = type_file ~raise "./contracts/closure-3.ligo" in
  let _ =
    let make_expect n = 49 + n in
    expect_eq_n_int ~raise program_3 "foobar" make_expect
  in
  let _ =
    let make_expect n = 45 + n in
    expect_eq_n_int ~raise program_2 "foobar" make_expect
  in
  let () =
    let make_expect n = 2 * n in
    expect_eq_n_int ~raise program_1 "foo" make_expect
  in
  let _ =
    let make_expect n = 4 * n in
    expect_eq_n_int ~raise program "toto" make_expect
  in
  ()
*)

let closure_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/closure.mligo" in
  let _ =
    let input = e_int ~loc 0 in
    let expected = e_int ~loc 25 in
    expect_eq ~raise program "test" input expected
  in
  ()


let closure_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/closure.jsligo" in
  let _ =
    let input = e_int ~loc 0 in
    let expected = e_int ~loc 25 in
    expect_eq ~raise program "test" input expected
  in
  ()


(*
let shadow ~raise () : unit =
  let program = type_file ~raise "./contracts/shadow.ligo" in
  let make_expect _ = 0 in
  expect_eq_n_int ~raise program "foo" make_expect
*)

let shadowing ~raise () : unit =
  let program = type_file ~raise "./contracts/shadowing.mligo" in
  let _ =
    let input = e_constructor ~loc "A" (e_int ~loc 1) in
    let expected = e_list ~loc [ e_constructor ~loc "A" (e_int ~loc 1) ] in
    expect_eq ~raise program "main" input expected
  in
  ()


let higher_order ~raise f : unit =
  let program = type_file ~raise f in
  let make_expect n = n in
  let _ = expect_eq_n_int ~raise program "foobar" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar2" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar3" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar4" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar5" make_expect in
  (* let _ = applies_expect_eq_n_int program "foobar5" make_expect in *)
  ()


let shared_function ~raise f : unit =
  let program = type_file ~raise f in
  let () =
    let make_expect n = n + 1 in
    expect_eq_n_int ~raise program "inc" make_expect
  in
  let () =
    let make_expect n = n + 2 in
    expect_eq_n_int ~raise program "double_inc" make_expect
  in
  let () =
    let make_expect n = (2 * n) + 3 in
    expect_eq ~raise program "foo" (e_int ~loc 0) (e_int ~loc @@ make_expect 0)
  in
  ()


let bool_expression ~raise f : unit =
  let program = type_file ~raise f in
  let _ =
    let aux (name, f) = expect_eq_b_bool program name f in
    List.map
      ~f:aux
      [ ("or_true", fun b -> b || true)
      ; ("or_false", fun b -> b || false)
      ; ("and_true", fun b -> b && true)
      ; ("and_false", fun b -> b && false)
      ; ("not_bool", fun b -> not b)
      ]
  in
  ()


let arithmetic ~raise f : unit =
  let program = type_file ~raise f in
  let _ =
    let aux (name, f) = expect_eq_n_int ~raise program name f in
    List.map
      ~f:aux
      [ ("plus_op", fun n -> n + 42)
      ; ("minus_op", fun n -> n - 42)
      ; ("times_op", fun n -> n * 42)
      ; ("neg_op", fun n -> -n)
      ; ("neg_op_2", fun n -> -(n + 10))
      ]
  in
  let () = expect_eq_n_pos ~raise program "int_op" (e_nat ~loc) (e_int ~loc) in
  let () =
    expect_eq_n_pos ~raise program "mod_op" (e_int ~loc) (fun n -> e_nat ~loc (n mod 42))
  in
  let () =
    expect_eq_n_pos ~raise program "div_op" (e_int ~loc) (fun n -> e_int ~loc (n / 2))
  in
  let () =
    expect_eq_n_pos ~raise program "ediv_op" (e_int ~loc) (fun n ->
        e_some ~loc (e_pair ~loc (e_int ~loc (n / 2)) (e_nat ~loc (n mod 2))))
  in
  let () = expect_eq_evaluate ~raise program "mul_woo" (e_unit ~loc ()) in
  ()


let bitwise_arithmetic ~raise f : unit =
  let program = type_file ~raise f in
  let () = expect_eq ~raise program "or_op" (e_nat ~loc 7) (e_nat ~loc 7) in
  let () = expect_eq ~raise program "or_op" (e_nat ~loc 3) (e_nat ~loc 7) in
  let () = expect_eq ~raise program "or_op" (e_nat ~loc 2) (e_nat ~loc 6) in
  let () = expect_eq ~raise program "or_op" (e_nat ~loc 14) (e_nat ~loc 14) in
  let () = expect_eq ~raise program "or_op" (e_nat ~loc 10) (e_nat ~loc 14) in
  let () = expect_eq ~raise program "and_op" (e_nat ~loc 7) (e_nat ~loc 7) in
  let () = expect_eq ~raise program "and_op" (e_nat ~loc 3) (e_nat ~loc 3) in
  let () = expect_eq ~raise program "and_op" (e_nat ~loc 2) (e_nat ~loc 2) in
  let () = expect_eq ~raise program "and_op" (e_nat ~loc 14) (e_nat ~loc 6) in
  let () = expect_eq ~raise program "and_op" (e_nat ~loc 10) (e_nat ~loc 2) in
  let () = expect_eq ~raise program "xor_op" (e_nat ~loc 0) (e_nat ~loc 7) in
  let () = expect_eq ~raise program "xor_op" (e_nat ~loc 7) (e_nat ~loc 0) in
  let () = expect_eq ~raise program "lsl_op" (e_nat ~loc 1000) (e_nat ~loc 128000) in
  let () = expect_eq ~raise program "lsr_op" (e_nat ~loc 128000) (e_nat ~loc 1000) in
  ()


let string_arithmetic ~raise f : unit =
  let program = type_file ~raise f in
  let () = expect_eq ~raise program "length_op" (e_string ~loc "tata") (e_nat ~loc 4) in
  let () = expect_eq ~raise program "length_op" (e_string ~loc "foo") (e_nat ~loc 3) in
  let () =
    expect_eq ~raise program "concat_op" (e_string ~loc "foo") (e_string ~loc "foototo")
  in
  let () =
    expect_eq ~raise program "concat_op" (e_string ~loc "") (e_string ~loc "toto")
  in
  let () =
    expect_eq ~raise program "sub_op" (e_string ~loc "tata") (e_string ~loc "at")
  in
  let () = expect_eq ~raise program "sub_op" (e_string ~loc "foo") (e_string ~loc "oo") in
  let () = expect_fail ~raise program "sub_op" (e_string ~loc "ba") in
  ()


let bytes_arithmetic ~raise f : unit =
  let program = type_file ~raise f in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez ~loc "0f00" in
  let foototo =
    trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez ~loc "0f007070"
  in
  let toto = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez ~loc "7070" in
  let empty = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez ~loc "" in
  let tata =
    trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez ~loc "ff7a7aff"
  in
  let at = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez ~loc "7a7a" in
  let ba = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez ~loc "ba" in
  let () = expect_eq ~raise program "concat_op" foo foototo in
  let () = expect_eq ~raise program "concat_op" empty toto in
  let () = expect_eq ~raise program "slice_op" tata at in
  let () = expect_fail ~raise program "slice_op" foo in
  let () = expect_fail ~raise program "slice_op" ba in
  let b1 =
    Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman" foo
  in
  let () = expect_eq_core ~raise program "hasherman" foo b1 in
  let b3 =
    Test_helpers.run_typed_program_with_imperative_input
      ~raise
      program
      "hasherman"
      foototo
  in
  let () =
    trace_assert_fail_option ~raise (test_internal __LOC__)
    @@ Ast_core.Misc.assert_value_eq (b3, b1)
  in
  ()


let comparable_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/comparable.mligo" in
  let () =
    expect_eq
      ~raise
      program
      "address_"
      (e_address ~loc "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
      (e_bool ~loc false)
  in
  let () = expect_eq ~raise program "bool_" (e_bool ~loc true) (e_bool ~loc false) in
  let () =
    expect_eq ~raise program "bytes_" (e_bytes_string ~loc "deadbeaf") (e_bool ~loc false)
  in
  let () = expect_eq ~raise program "int_" (e_int ~loc 1) (e_bool ~loc false) in
  let () = expect_eq ~raise program "mutez_" (e_mutez ~loc 1) (e_bool ~loc false) in
  let () = expect_eq ~raise program "nat_" (e_nat ~loc 1) (e_bool ~loc false) in
  let () =
    expect_eq ~raise program "option_" (e_some ~loc (e_int ~loc 1)) (e_bool ~loc false)
  in
  (*
  let () = expect_eq ~raise program "sum_" (e_constructor ~loc "A" (e_int ~loc 1)) (e_bool ~loc false) in
  *)
  let () = expect_eq ~raise program "string_" (e_string ~loc "foo") (e_bool ~loc false) in
  let () =
    expect_eq ~raise program "timestamp_" (e_timestamp ~loc 101112) (e_bool ~loc false)
  in
  let () = expect_eq ~raise program "unit_" (e_unit ~loc ()) (e_bool ~loc false) in
  (*
  let () = expect_eq ~raise program "sum" (e_constructor ~loc "A" (e_int ~loc 1)) (e_bool ~loc false) in
  *)
  let open Tezos_crypto in
  let pkh, pk, sk = Signature.generate_key () in
  let key_hash = Signature.Public_key_hash.to_b58check @@ pkh in
  let () =
    expect_eq ~raise program "key_hash_" (e_key_hash ~loc key_hash) (e_bool ~loc false)
  in
  let key = Signature.Public_key.to_b58check @@ pk in
  let () = expect_eq ~raise program "key_" (e_key ~loc key) (e_bool ~loc false) in
  let signed =
    Signature.to_b58check @@ Signature.sign sk (Bytes.of_string "hello world")
  in
  let () =
    expect_eq ~raise program "signature_" (e_signature ~loc signed) (e_bool ~loc false)
  in
  let chain_id =
    Tezos_crypto.Base58.simple_encode
      Tezos_base__TzPervasives.Chain_id.b58check_encoding
      Tezos_base__TzPervasives.Chain_id.zero
  in
  let () =
    expect_eq ~raise program "chain_id_" (e_chain_id ~loc chain_id) (e_bool ~loc false)
  in
  let pair = e_pair ~loc (e_int ~loc 1) (e_int ~loc 2) in
  let () = expect_eq ~raise program "comp_pair" pair (e_bool ~loc false) in
  (* let tuple = e_tuple ~loc [e_int 1; e_int ~loc 2; e_int ~loc 3] in
  let () = expect_string_failwith program "uncomp_pair_1" tuple "" in
  let pair = e_pair ~loc pair (e_int ~loc 3) in
  let () = expect_string_failwith program "uncomp_pair_2" pair "" in *)
  let comb = e_pair ~loc (e_int ~loc 3) (e_pair ~loc (e_int ~loc 1) (e_nat ~loc 2)) in
  let () = expect_eq ~raise program "comb_record" comb (e_bool ~loc false) in
  ()


let crypto ~raise f : unit =
  let program = type_file ~raise f in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez ~loc "0f00" in
  let foototo =
    trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez ~loc "0f007070"
  in
  let b1 =
    Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foo
  in
  let () = expect_eq_core ~raise program "hasherman512" foo b1 in
  let b2 =
    Test_helpers.run_typed_program_with_imperative_input
      ~raise
      program
      "hasherman512"
      foototo
  in
  let () =
    trace_assert_fail_option ~raise (test_internal __LOC__)
    @@ Ast_core.Misc.assert_value_eq (b2, b1)
  in
  let b4 =
    Test_helpers.run_typed_program_with_imperative_input
      ~raise
      program
      "hasherman_blake"
      foo
  in
  let () = expect_eq_core ~raise program "hasherman_blake" foo b4 in
  let b5 =
    Test_helpers.run_typed_program_with_imperative_input
      ~raise
      program
      "hasherman_blake"
      foototo
  in
  let () =
    trace_assert_fail_option ~raise (test_internal __LOC__)
    @@ Ast_core.Misc.assert_value_eq (b5, b4)
  in
  ()


let set_arithmetic ~raise f : unit =
  let program = type_file ~raise f in
  let () =
    expect_eq
      ~raise
      program
      "literal_op"
      (e_unit ~loc ())
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
  in
  let () =
    expect_eq
      ~raise
      program
      "size_op"
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
      (e_nat ~loc 3)
  in
  let () =
    expect_eq
      ~raise
      program
      "add_op"
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar" ])
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
  in
  let () =
    expect_eq
      ~raise
      program
      "add_op"
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
  in
  let () =
    expect_eq
      ~raise
      program
      "remove_op"
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar" ])
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar" ])
  in
  let () =
    expect_eq
      ~raise
      program
      "remove_op"
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar" ])
  in
  let () =
    expect_eq
      ~raise
      program
      "remove_deep"
      (e_pair
         ~loc
         (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
         (e_nat ~loc 42))
      (e_pair
         ~loc
         (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar" ])
         (e_nat ~loc 42))
  in
  let () =
    expect_eq
      ~raise
      program
      "patch_op"
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar" ])
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
  in
  let () =
    expect_eq
      ~raise
      program
      "patch_op_deep"
      (e_pair
         ~loc
         (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar" ])
         (e_nat ~loc 42))
      (e_pair
         ~loc
         (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
         (e_nat ~loc 42))
  in
  let () =
    expect_eq
      ~raise
      program
      "mem_op"
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar"; e_string ~loc "foobar" ])
      (e_bool ~loc true)
  in
  let () =
    expect_eq
      ~raise
      program
      "mem_op"
      (e_set ~loc [ e_string ~loc "foo"; e_string ~loc "bar" ])
      (e_bool ~loc false)
  in
  let () =
    expect_eq
      ~raise
      program
      "iter_op"
      (e_set ~loc [ e_int ~loc 2; e_int ~loc 4; e_int ~loc 7 ])
      (e_int ~loc 0)
  in
  (* capture of non constant variable
  let () =
    expect_eq ~raise program "iter_op_with_effect"
      (e_set ~loc [e_int 2 ; e_int ~loc 4 ; e_int ~loc 7])
      (e_int ~loc 13) in *)
  let () =
    expect_eq
      ~raise
      program
      "fold_op"
      (e_set ~loc [ e_int ~loc 4; e_int ~loc 10 ])
      (e_list ~loc [ e_int ~loc 10; e_int ~loc 4 ])
  in
  let () =
    expect_eq
      ~raise
      program
      "fold_right"
      (e_set ~loc [ e_int ~loc 4; e_int ~loc 10 ])
      (e_list ~loc [ e_int ~loc 4; e_int ~loc 10 ])
  in
  ()


(*
let unit_expression ~raise () : unit =
  let program = type_file ~raise "./contracts/unit.ligo" in
  expect_eq_evaluate ~raise program "u" (e_unit ~loc ())

let string_expression ~raise () : unit =
  let program = type_file ~raise "./contracts/string.ligo" in
  let _ = expect_eq_evaluate ~raise program "s" (e_string ~loc "toto") in
  expect_eq_evaluate ~raise program "y" (e_string ~loc "foototobar")
*)

let include_ ~raise f : unit =
  let program = type_file ~raise f in
  expect_eq_evaluate ~raise program "bar" (e_int ~loc 144)


let include_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/includer.mligo" in
  expect_eq_evaluate ~raise program "bar" (e_int ~loc 144)


let include_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/includer.jsligo" in
  expect_eq_evaluate ~raise program "bar" (e_int ~loc 144)


let modules ~raise program : unit =
  let () = expect_eq_evaluate ~raise program "toto" (e_int ~loc 42) in
  expect_eq
    ~raise
    program
    "add"
    (e_pair ~loc (e_int ~loc 1) (e_int ~loc 2))
    (e_int ~loc 3)


(* Not called? -- Christian
let modules_ligo ~raise () : unit =
  let program = type_file ~raise "./contracts/modules.ligo" in
  modules ~raise program
*)

let modules_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/modules.mligo" in
  modules ~raise program


let modules_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/modules.jsligo" in
  modules ~raise program


let record_ez_int names n =
  e_record_ez ~loc @@ List.map ~f:(fun x -> x, e_int ~loc n) names


let tuple_ez_int names n = e_tuple ~loc @@ List.map ~f:(fun _ -> e_int ~loc n) names

let multiple_parameters ~raise f : unit =
  let program = type_file ~raise f in
  let aux ((name : string), make_input, make_output) =
    let make_output' n = e_int ~loc @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ =
    List.map
      ~f:aux
      [ ("ab", tuple_ez_int [ "a"; "b" ], fun n -> 2 * n)
      ; ("abcd", tuple_ez_int [ "a"; "b"; "c"; "d" ], fun n -> (4 * n) + 2)
      ; ("abcde", tuple_ez_int [ "a"; "b"; "c"; "d"; "e" ], fun n -> (2 * n) + 3)
      ]
  in
  ()


let multiple_parameters_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/multiple-parameters.mligo" in
  let aux ((name : string), make_input, make_output) =
    let make_output' n = e_int ~loc @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ =
    List.map
      ~f:aux
      [ (* Didn't include the other tests because they're probably not necessary *)
        ("abcde", tuple_ez_int [ "a"; "b"; "c"; "d"; "e" ], fun n -> (2 * n) + 3)
      ]
  in
  ()


let multiple_parameters_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/multiple-parameters.jsligo" in
  let aux ((name : string), make_input, make_output) =
    let make_output' n = e_int ~loc @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ =
    List.map
      ~f:aux
      [ (* Didn't include the other tests because they're probably not necessary *)
        ("abcde", tuple_ez_int [ "a"; "b"; "c"; "d"; "e" ], fun n -> (2 * n) + 3)
      ]
  in
  ()


let record ~raise f : unit =
  let program = type_file ~raise f in
  let () =
    let expected = record_ez_int [ "foo"; "bar" ] 0 in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let () = expect_eq_evaluate ~raise program "a" (e_int ~loc 42) in
    let () = expect_eq_evaluate ~raise program "b" (e_int ~loc 142) in
    let () = expect_eq_evaluate ~raise program "c" (e_int ~loc 242) in
    ()
  in
  let () =
    let make_input = record_ez_int [ "foo"; "bar" ] in
    let make_expected n = e_int ~loc (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = record_ez_int [ "foo"; "bar" ] in
    let make_expected n =
      e_record_ez ~loc [ "foo", e_int ~loc 256; "bar", e_int ~loc n ]
    in
    expect_eq_n ~raise program "modify" make_input make_expected
  in
  let () =
    let make_input = record_ez_int [ "a"; "b"; "c" ] in
    let make_expected n =
      e_record_ez ~loc [ "a", e_int ~loc n; "b", e_int ~loc 2048; "c", e_int ~loc 42 ]
    in
    expect_eq_n ~raise program "modify_abc" make_input make_expected
  in
  let () =
    let expected = record_ez_int [ "a"; "b"; "c"; "d"; "e" ] 23 in
    expect_eq_evaluate ~raise program "br" expected
  in
  let () =
    let make_input n = e_record_ez ~loc [ "inner", record_ez_int [ "a"; "b"; "c" ] n ] in
    let make_expected n =
      e_record_ez
        ~loc
        [ ( "inner"
          , e_record_ez
              ~loc
              [ "a", e_int ~loc n; "b", e_int ~loc 2048; "c", e_int ~loc n ] )
        ]
    in
    expect_eq_n ~raise program "modify_inner" make_input make_expected
  in
  ()


let tuple ~raise f : unit =
  let program = type_file ~raise f in
  let ez n = e_tuple ~loc (List.map ~f:(e_int ~loc) n) in
  let () =
    let expected = ez [ 0; 0 ] in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let make_input n = ez [ n; n ] in
    let make_expected n = e_int ~loc (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input n = ez [ n; 2 * n; n ] in
    let make_expected n = e_int ~loc (2 * n) in
    expect_eq_n ~raise program "projection_abc" make_input make_expected
  in
  let () =
    let make_input n = ez [ n; n; n ] in
    let make_expected n = ez [ n; 2048; n ] in
    expect_eq ~raise program "modify_abc" (make_input 12) (make_expected 12)
  in
  let () =
    let make_input n = ez [ n; n; n ] in
    let make_expected n = ez [ n; 2048; n ] in
    expect_eq_n ~raise program "modify_abc" make_input make_expected
  in
  let () =
    let expected = ez [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ] in
    expect_eq_evaluate ~raise program "br" expected
  in
  let () =
    let make_input n = ez [ n; n; n; n; n; n; n; n; n; n; n; n ] in
    let make_expected n = ez [ n; n; n; n; n; n; n; n; n; n; n; 2048 ] in
    expect_eq_n ~raise program "update" make_input make_expected
  in
  ()


let option ~raise f : unit =
  let program = type_file ~raise f in
  let () =
    let expected = e_some ~loc (e_int ~loc 42) in
    expect_eq_evaluate ~raise program "s" expected
  in
  let () =
    let expected = e_typed_none ~loc (t_int ~loc ()) in
    expect_eq_evaluate ~raise program "n" expected
  in
  let () =
    let expected = e_int ~loc 42 in
    expect_eq_evaluate ~raise program "i" expected
  in
  let () =
    let expected = e_typed_none ~loc (t_int ~loc ()) in
    expect_eq ~raise program "assign" (e_int ~loc 12) expected
  in
  ()


let map ~raise f : unit =
  let program = type_file ~raise f in
  let ez lst =
    let lst' = List.map ~f:(fun (x, y) -> e_int ~loc x, e_int ~loc y) lst in
    e_typed_map ~loc lst' (t_int ~loc ()) (t_int ~loc ())
  in
  let () =
    let make_input n =
      let m = ez [ 23, 0; 42, 0 ] in
      e_tuple ~loc [ e_int ~loc n; m ]
    in
    let make_expected n = ez [ 23, n; 42, 0 ] in
    expect_eq_n_pos_small ~raise program "set_" make_input make_expected
  in
  let () =
    let input = e_pair ~loc (e_int ~loc 23) (ez [ 42, 42 ]) in
    let expected = ez [ 23, 23; 42, 42 ] in
    expect_eq ~raise program "add" input expected
  in
  let () =
    let input = ez [ 23, 23; 42, 42 ] in
    let expected = ez [ 23, 23 ] in
    expect_eq ~raise program "rm" input expected
  in
  let () =
    let input = ez [ 0, 0; 1, 1; 2, 2 ] in
    let expected = ez [ 0, 5; 1, 6; 2, 7 ] in
    expect_eq ~raise program "patch_" input expected
  in
  let () =
    let input = e_pair ~loc (ez [ 0, 0; 1, 1; 2, 2 ]) (e_nat ~loc 10) in
    let expected = e_pair ~loc (ez [ 0, 0; 1, 9; 2, 2 ]) (e_nat ~loc 10) in
    expect_eq ~raise program "patch_deep" input expected
  in
  let () =
    let make_input n = ez List.(map ~f:(fun x -> x, x) @@ range 0 n) in
    let make_expected = e_nat ~loc in
    expect_eq_n_strict_pos_small ~raise program "size_" make_input make_expected
  in
  let () =
    let make_input n = ez [ 23, n; 42, 4 ] in
    let make_expected _ = e_some ~loc @@ e_int ~loc 4 in
    expect_eq_n ~raise program "get" make_input make_expected
  in
  let () =
    let input_map = ez [ 23, 10; 42, 4 ] in
    expect_eq
      ~raise
      program
      "mem"
      (e_tuple ~loc [ e_int ~loc 23; input_map ])
      (e_bool ~loc true)
  in
  let () =
    let input_map = ez [ 23, 10; 42, 4 ] in
    expect_eq
      ~raise
      program
      "mem"
      (e_tuple ~loc [ e_int ~loc 1000; input_map ])
      (e_bool ~loc false)
  in
  let () =
    expect_eq_evaluate
      ~raise
      program
      "empty_map"
      (e_annotation ~loc (e_map ~loc []) (t_map ~loc (t_int ~loc ()) (t_int ~loc ())))
  in
  let () =
    let expected = ez @@ List.map ~f:(fun x -> x, 23) [ 144; 51; 42; 120; 421 ] in
    expect_eq_evaluate ~raise program "map1" expected
  in
  let () =
    let expected = ez [ 23, 0; 42, 0 ] in
    expect_eq_evaluate ~raise program "map2" expected
  in
  let () =
    let input = ez [ 1, 1; 2, 2; 3, 3 ] in
    let expected = e_unit ~loc () in
    expect_eq ~raise program "iter_op" input expected
  in
  let () =
    let input = ez [ 1, 10; 2, 20; 3, 30 ] in
    let expected = ez [ 1, 11; 2, 21; 3, 31 ] in
    expect_eq ~raise program "map_op" input expected
  in
  let () =
    let input = ez [ 1, 10; 2, 20; 3, 30 ] in
    let expected = e_int ~loc 76 in
    expect_eq ~raise program "fold_op" input expected
  in
  let () =
    let input = ez [ 2, 20; 42, 10 ] in
    let expected = ez [ 2, 20; 32, 16 ] in
    expect_eq ~raise program "deep_op" input expected
  in
  ()


let big_map ~raise f : unit =
  let program = type_file ~raise f in
  let ez lst =
    let lst' = List.map ~f:(fun (x, y) -> e_int ~loc x, e_int ~loc y) lst in
    e_typed_big_map ~loc lst' (t_int ~loc ()) (t_int ~loc ())
  in
  let () =
    let make_input n =
      let m = ez [ 23, 0; 42, 0 ] in
      e_tuple ~loc [ e_int ~loc n; m ]
    in
    let make_expected n = ez [ 23, n; 42, 0 ] in
    expect_eq_n_pos_small ~raise program "set_" make_input make_expected
  in
  let () =
    let input = e_pair ~loc (e_int ~loc 23) (ez [ 42, 42 ]) in
    let expected = ez [ 23, 23; 42, 42 ] in
    expect_eq ~raise program "add" input expected
  in
  let () =
    let make_input n = ez [ 23, n; 42, 4 ] in
    let make_expected _ = e_some ~loc @@ e_int ~loc 4 in
    expect_eq_n ~raise program "get" make_input make_expected
  in
  let () =
    let input = ez [ 23, 23; 42, 42 ] in
    let expected = ez [ 23, 23 ] in
    expect_eq ~raise program "rm" input expected
  in
  ()


(*
let list ~raise () : unit =
  Format.printf "Pre_type \n%!";
  let program = type_file ~raise "./contracts/list.ligo" in
  let ez lst =
    let lst' = List.map ~f:(e_int ~loc) lst in
    e_typed_list ~loc lst' (t_int ~loc ())
  in
  Format.printf "Post_type \n%!";
  let () =
    let expected = ez [ 23; 42 ] in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let expected = ez [ 144; 23; 42 ] in
    expect_eq_evaluate ~raise program "fb2" expected
  in
  let () =
    let expected = ez [ 688; 144; 23; 42 ] in
    expect_eq_evaluate ~raise program "fb3" expected
  in
  let () =
    let expected = e_some ~loc @@ e_int ~loc 23 in
    expect_eq_evaluate ~raise program "fb_head" expected
  in
  let () =
    let expected = e_some ~loc @@ ez [ 42 ] in
    expect_eq_evaluate ~raise program "fb_tail" expected
  in
  let () =
    let make_input n = ez @@ List.range 0 n in
    let make_expected = e_nat ~loc in
    expect_eq_n_strict_pos_small ~raise program "size_" make_input make_expected
  in
  let () =
    let expected = ez [ 144; 51; 42; 120; 421 ] in
    expect_eq_evaluate ~raise program "bl" expected
  in
  let () =
    expect_eq
      ~raise
      program
      "fold_op"
      (e_list ~loc [ e_int ~loc 2; e_int ~loc 4; e_int ~loc 7 ])
      (e_int ~loc 23)
  in
  (* not working since purification (problem with effect in out of iter
  let () =
    expect_eq ~raise program "iter_op"
      (e_list ~loc [e_int 2 ; e_int ~loc 4 ; e_int ~loc 7])
      (e_int ~loc 13)
  in
  *)
  let () =
    expect_eq
      ~raise
      program
      "map_op"
      (e_list ~loc [ e_int ~loc 2; e_int ~loc 4; e_int ~loc 7 ])
      (e_list ~loc [ e_int ~loc 3; e_int ~loc 5; e_int ~loc 8 ])
  in
  let () = expect_eq_evaluate ~raise program "find_x" (e_none ~loc ()) in
  let () = expect_eq_evaluate ~raise program "find_y4" (e_some ~loc (e_int ~loc 4)) in
  let () = expect_eq_evaluate ~raise program "find_y6" (e_none ~loc ()) in
  let () = expect_eq_evaluate ~raise program "find_z2" (e_some ~loc (e_int ~loc 2)) in
  ()
*)

let condition ~raise f : unit =
  let program = type_file ~raise f in
  let make_input = e_int ~loc in
  let make_expected n = e_int ~loc (if n = 2 then 42 else 0) in
  let () = expect_eq_n ~raise program "simple" make_input make_expected in
  let () = expect_eq_n ~raise program "annot" make_input make_expected in
  let () = expect_eq_n ~raise program "shadow" make_input make_expected in
  ()


let sequence_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/sequence.mligo" in
  expect_eq ~raise program "y" (e_unit ~loc ()) (e_nat ~loc 1)


let eq_bool_common ~raise program =
  let _ =
    List.map
      ~f:(fun (a, b, expected) ->
        expect_eq_twice
          ~raise
          program
          "main"
          (e_bool ~loc a)
          (e_bool ~loc b)
          (e_int ~loc expected))
      [ false, false, 999; false, true, 1; true, false, 1; true, true, 999 ]
  in
  ()


let eq_bool ~raise f : unit =
  let program = type_file ~raise f in
  eq_bool_common ~raise program


let eq_bool_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/eq_bool.mligo" in
  eq_bool_common ~raise program


let eq_bool_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/eq_bool.jsligo" in
  eq_bool_common ~raise program


(*
let loop1 ~raise () : unit =
  let _program = type_file ~raise "./contracts/loop1.ligo" in
  ()

let loop2 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop2.ligo" in
  let () =
    let make_input = e_nat ~loc in
    let make_expected = e_nat ~loc in
    expect_eq_n_pos ~raise program "dummy" make_input make_expected
  in
  ()

let loop3 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop3.ligo" in
  let () =
    let make_input = e_nat ~loc in
    let make_expected = e_nat ~loc in
    expect_eq_n_pos_mid ~raise program "counter" make_input make_expected
  in
  ()

let loop4 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop4.ligo" in
  let () =
    let make_input = e_nat ~loc in
    let make_expected n = e_nat ~loc (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "while_sum" make_input make_expected
  in
  let () =
    let make_input = e_nat ~loc in
    let make_expected n = e_nat ~loc (if n = 0 then 0 else 1) in
    expect_eq_n_pos_mid ~raise program "while_record" make_input make_expected
  in
  ()

let loop5 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop5.ligo" in
  let () =
    let make_input = e_nat ~loc in
    let make_expected n = e_int ~loc (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "for_sum" make_input make_expected
  in
  ()

let loop6 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop6.ligo" in
  let () =
    let make_input = e_nat ~loc in
    let make_expected n = e_int ~loc (n * n) in
    expect_eq_n_pos_mid ~raise program "for_sum_step" make_input make_expected
  in
  ()

let loop7 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop7.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_pair ~loc (e_int ~loc 3) (e_string ~loc "totototo") in
    expect_eq ~raise program "for_collection_list" input expected
  in
  ()

let loop8 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop8.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_pair ~loc (e_int ~loc 6) (e_string ~loc "totototo") in
    expect_eq ~raise program "for_collection_set" input expected
  in
  ()

let loop9 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop9.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_pair ~loc (e_int ~loc 6) (e_string ~loc "123") in
    expect_eq ~raise program "for_collection_map_kv" input expected
  in
  ()

let loop10 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop10.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_int ~loc 0 in
    expect_eq ~raise program "for_collection_empty" input expected
  in
  ()

let loop11 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop11.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_int ~loc 13 in
    expect_eq ~raise program "for_collection_if_and_local_var" input expected
  in
  ()

let loop12 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop12.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_int ~loc 1020 in
    expect_eq ~raise program "for_collection_rhs_capture" input expected
  in
  ()

let loop13 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop13.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_int ~loc 1040 in
    expect_eq ~raise program "for_collection_proc_call" input expected
  in
  ()

let loop14 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop14.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_int ~loc 20 in
    expect_eq ~raise program "for_collection_comp_with_acc" input expected
  in
  ()

let loop15 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop15.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected =
      e_pair
        ~loc
        (e_int ~loc 24)
        (e_string
           ~loc
           "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 \
            one,two 3 one,two ")
    in
    expect_eq ~raise program "nested_for_collection" input expected
  in
  ()

let loop16 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop16.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_pair ~loc (e_int ~loc 24) (e_string ~loc "123123123") in
    expect_eq ~raise program "nested_for_collection_local_var" input expected
  in
  ()

let loop17 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_pair ~loc (e_bool ~loc true) (e_int ~loc 4) in
    expect_eq ~raise program "inner_capture_in_conditional_block" input expected
  in
  ()

let loop18 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop.ligo" in
  let input = e_unit ~loc () in
  let () =
    let ez lst =
      let lst' = List.map ~f:(fun (x, y) -> e_string ~loc x, e_int ~loc y) lst in
      e_typed_map ~loc lst' (t_string ~loc ()) (t_int ~loc ())
    in
    let expected = ez [ "I", 12; "am", 12; "foo", 12 ] in
    expect_eq ~raise program "for_collection_with_patches" input expected
  in
  ()

let loop19 ~raise () : unit =
  let program = type_file ~raise "./contracts/loop19.ligo" in
  let () =
    let make_input = e_int ~loc in
    let make_expected n = e_int ~loc (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "nested_loops" make_input make_expected
  in
  ()

let nested_for_loop ~raise () : unit =
  let program = type_file ~raise "./contracts/nested_for_loop.ligo" in
  let () =
    let make_input = e_int ~loc in
    let make_expected n = e_int ~loc (n * n * n) in
    expect_eq_n_pos_mid ~raise program "main" make_input make_expected
  in
  ()

let loop ~raise () : unit =
  let program = type_file ~raise "./contracts/loop.ligo" in
  let () =
    let make_input = e_nat ~loc in
    let make_expected = e_nat ~loc in
    expect_eq_n_pos ~raise program "dummy" make_input make_expected
  in
  let () =
    let make_input = e_nat ~loc in
    let make_expected = e_nat ~loc in
    expect_eq_n_pos_mid ~raise program "counter" make_input make_expected
  in
  let () =
    let make_input = e_nat ~loc in
    let make_expected n = e_nat ~loc (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "while_sum" make_input make_expected
  in
  let () =
    let make_input = e_nat ~loc in
    let make_expected n = e_int ~loc (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "for_sum" make_input make_expected
  in
  let () =
    let make_input = e_nat ~loc in
    let make_expected n = e_int ~loc (n * n) in
    expect_eq_n_pos_mid ~raise program "for_sum_step" make_input make_expected
  in
  let input = e_unit ~loc () in
  let () =
    let expected = e_pair ~loc (e_int ~loc 3) (e_string ~loc "totototo") in
    expect_eq ~raise program "for_collection_list" input expected
  in
  let () =
    let expected = e_pair ~loc (e_int ~loc 6) (e_string ~loc "totototo") in
    expect_eq ~raise program "for_collection_set" input expected
  in
  let () =
    let expected = e_pair ~loc (e_int ~loc 6) (e_string ~loc "123") in
    expect_eq ~raise program "for_collection_map_kv" input expected
  in
  let () =
    let expected = e_int ~loc 0 in
    expect_eq ~raise program "for_collection_empty" input expected
  in
  let () =
    let expected = e_int ~loc 13 in
    expect_eq ~raise program "for_collection_if_and_local_var" input expected
  in
  let () =
    let expected = e_int ~loc 1020 in
    expect_eq ~raise program "for_collection_rhs_capture" input expected
  in
  let () =
    let expected = e_int ~loc 1040 in
    expect_eq ~raise program "for_collection_proc_call" input expected
  in
  let () =
    let expected = e_int ~loc 20 in
    expect_eq ~raise program "for_collection_comp_with_acc" input expected
  in
  let () =
    let expected =
      e_pair
        ~loc
        (e_int ~loc 24)
        (e_string
           ~loc
           "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 \
            one,two 3 one,two ")
    in
    expect_eq ~raise program "nested_for_collection" input expected
  in
  let () =
    let expected = e_pair ~loc (e_int ~loc 24) (e_string ~loc "123123123") in
    expect_eq ~raise program "nested_for_collection_local_var" input expected
  in
  let () =
    let expected = e_pair ~loc (e_bool ~loc true) (e_int ~loc 4) in
    expect_eq ~raise program "inner_capture_in_conditional_block" input expected
  in
  let () =
    let ez lst =
      let lst' = List.map ~f:(fun (x, y) -> e_string ~loc x, e_int ~loc y) lst in
      e_typed_map ~loc lst' (t_string ~loc ()) (t_int ~loc ())
    in
    let expected = ez [ "I", 12; "am", 12; "foo", 12 ] in
    expect_eq ~raise program "for_collection_with_patches" input expected
  in
  ()
*)

(* Don't know how to assert that a parse error happens in this test framework

let for_fail ~raise () : unit =
  let program = type_file "./contracts/for_fail.ligo" in
  let () = expect_fail program "main" (e_nat ~loc 0)
  in () *)

let loop_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/loop.mligo" in
  let () =
    let input = e_int ~loc 0 in
    let expected = e_int ~loc 100 in
    expect_eq ~raise program "counter_simple" input expected
  in
  let () =
    let input = e_int ~loc 100 in
    let expected = e_int ~loc 5050 in
    expect_eq ~raise program "counter" input expected
  in
  let () =
    let input = e_int ~loc 100 in
    let expected = e_int ~loc 10000 in
    expect_eq ~raise program "counter_nest" input expected
  in
  ()


let loop_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/loop.jsligo" in
  let () =
    let input = e_int ~loc 0 in
    let expected = e_int ~loc 100 in
    expect_eq ~raise program "counter_simple" input expected
  in
  let () =
    let input = e_int ~loc 100 in
    let expected = e_int ~loc 5050 in
    expect_eq ~raise program "counter" input expected
  in
  let () =
    let input = e_int ~loc 100 in
    let expected = e_int ~loc 10000 in
    expect_eq ~raise program "counter_nest" input expected
  in
  let () =
    let input = e_variable_ez ~loc "testmap" in
    let expected =
      e_list
        ~loc
        [ e_pair ~loc (e_int ~loc 2) (e_int ~loc 4)
        ; e_pair ~loc (e_int ~loc 1) (e_int ~loc 2)
        ; e_pair ~loc (e_int ~loc 0) (e_int ~loc 1)
        ]
    in
    expect_eq ~raise program "entries" input expected
  in
  ()


let loop2_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/loop2.jsligo" in
  (* let () =
    let make_input = e_nat ~loc in
    let make_expected = e_nat ~loc in
    expect_eq_n_pos program "dummy" make_input make_expected in *)
  let () =
    let make_input = e_nat ~loc in
    let make_expected = e_nat ~loc in
    expect_eq_n_pos_mid ~raise program "counter" make_input make_expected
  in
  (* let () =
    let make_input = e_nat ~loc in
    let make_expected = fun n -> e_nat ~loc (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "while_sum" make_input make_expected in
  let () =
    let make_input = e_nat ~loc in
    let make_expected = fun n -> e_int ~loc (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "for_sum" make_input make_expected in
  let () =
    let make_input = e_nat ~loc in
    let make_expected = fun n -> e_int ~loc (n * n) in
    expect_eq_n_pos_mid program "for_sum_step" make_input make_expected in
  let input = e_unit ~loc () in
  let () =
    let expected = e_pair ~loc (e_int ~loc 3) (e_string ~loc "totototo") in
    expect_eq ~raise program "for_collection_list" input expected in
  let () =
    let expected = e_pair ~loc (e_int ~loc 6) (e_string ~loc "totototo") in
    expect_eq ~raise program "for_collection_set" input expected in
  let () =
    let expected = e_pair ~loc (e_int ~loc 6) (e_string ~loc "123") in
    expect_eq ~raise program "for_collection_map_kv" input expected in
  let () =
    let expected = (e_int ~loc 0) in
    expect_eq ~raise program "for_collection_empty" input expected in
  let () =
    let expected = (e_int ~loc 13) in
    expect_eq ~raise program "for_collection_if_and_local_var" input expected in
  let () =
    let expected = (e_int ~loc 1020) in
    expect_eq ~raise program "for_collection_rhs_capture" input expected in
  let () =
    let expected = (e_int ~loc 1040) in
    expect_eq ~raise program "for_collection_proc_call" input expected in
  let () =
    let expected = (e_int ~loc 20) in
    expect_eq ~raise program "for_collection_comp_with_acc" input expected in
  let () =
    let expected = e_pair ~loc (e_int ~loc 24)
      (e_string ~loc "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two ") in
    expect_eq ~raise program "nested_for_collection" input expected in
  let () =
    let expected = e_pair ~loc (e_int ~loc 24)
      (e_string ~loc "123123123") in
    expect_eq ~raise program "nested_for_collection_local_var" input expected in
  let () =
    let expected = e_pair ~loc (e_bool ~loc true) (e_int ~loc 4) in
    expect_eq ~raise program "inner_capture_in_conditional_block"  input expected in
  let () =
    let ez lst =
      let lst' = List.map ~f:(fun (x, y) -> e_string ~loc x, e_int ~loc y) lst in
        e_typed ~loc_map lst' (t_string ()) (t_int ())
    in
    let expected = ez [ ("I" , 12) ; ("am" , 12) ; ("foo" , 12) ] in
    expect_eq ~raise program "for_collection_with_patches" input expected in *)
  ()


let matching ~raise () : unit =
  let program = type_file ~raise "./contracts/match.ligo" in
  let () =
    let make_input = e_int ~loc in
    let make_expected n = e_int ~loc (if n = 2 then 42 else 0) in
    expect_eq_n ~raise program "match_bool" make_input make_expected
  in
  let () =
    let make_input = e_int ~loc in
    let make_expected n = e_int ~loc (if n = 2 then 42 else 0) in
    expect_eq_n ~raise program "match_expr_bool" make_input make_expected
  in
  let () =
    let aux n =
      let input =
        match n with
        | Some s -> e_some ~loc (e_int ~loc s)
        | None -> e_typed_none ~loc (t_int ~loc ())
      in
      let expected =
        e_int
          ~loc
          (match n with
          | Some s -> s
          | None -> 23)
      in
      expect_eq ~raise program "match_option" input expected
    in
    List.iter ~f:aux [ Some 0; Some 2; Some 42; Some 163; Some (-1); None ]
  in
  let () =
    let aux n =
      let input =
        match n with
        | Some s -> e_some ~loc (e_int ~loc s)
        | None -> e_typed_none ~loc (t_int ~loc ())
      in
      let expected =
        e_int
          ~loc
          (match n with
          | Some s -> s
          | None -> 42)
      in
      expect_eq ~raise program "match_expr_option" input expected
    in
    List.iter ~f:aux [ Some 0; Some 2; Some 42; Some 163; Some (-1); None ]
  in
  let () =
    let aux lst =
      e_annotation
        ~loc
        (e_list ~loc @@ List.map ~f:(e_int ~loc) lst)
        (t_list ~loc (t_int ~loc ()))
    in
    let () =
      expect_eq ~raise program "match_expr_list" (aux [ 14; 2; 3 ]) (e_int ~loc 14)
    in
    let () =
      expect_eq ~raise program "match_expr_list" (aux [ 13; 2; 3 ]) (e_int ~loc 13)
    in
    let () = expect_eq ~raise program "match_expr_list" (aux []) (e_int ~loc (-1)) in
    ()
  in
  ()


let counter_contract ~raise f : unit =
  let program = type_file ~raise f in
  let make_input n = e_int ~loc n, e_int ~loc 42 in
  let make_expected n =
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (42 + n))
  in
  expect_eq_n_twice ~raise program "main" make_input make_expected


let super_counter_contract ~raise f : unit =
  let program = type_file ~raise f in
  let make_input n =
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_constructor ~loc action (e_int ~loc n), e_int ~loc 42
  in
  let make_expected n =
    let op = if n mod 2 = 0 then ( + ) else ( - ) in
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (op 42 n))
  in
  expect_eq_n_twice ~raise program "main" make_input make_expected


(*
let dispatch_counter_contract ~raise () : unit =
  let program = type_file ~raise "./contracts/dispatch-counter.ligo" in
  let make_input n =
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair ~loc (e_constructor ~loc action (e_int ~loc n)) (e_int ~loc 42)
  in
  let make_expected n =
    let op = if n mod 2 = 0 then ( + ) else ( - ) in
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (op 42 n))
  in
  expect_eq_n ~raise program "main" make_input make_expected

let failwith_ligo ~raise () : unit =
  let program = type_file ~raise "./contracts/failwith.ligo" in
  let should_fail = expect_fail ~raise program "main" in
  let should_work input =
    expect_eq
      ~raise
      program
      "main"
      input
      (e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_unit ~loc ()))
  in
  let _ =
    should_work (e_pair ~loc (e_constructor ~loc "Zero" (e_nat ~loc 0)) (e_unit ~loc ()))
  in
  let _ =
    should_fail (e_pair ~loc (e_constructor ~loc "Zero" (e_nat ~loc 1)) (e_unit ~loc ()))
  in
  let _ =
    should_work (e_pair ~loc (e_constructor ~loc "Pos" (e_nat ~loc 1)) (e_unit ~loc ()))
  in
  let _ =
    should_fail (e_pair ~loc (e_constructor ~loc "Pos" (e_nat ~loc 0)) (e_unit ~loc ()))
  in
  let should_fail input = expect_fail ~raise program "foobar" (e_int ~loc input) in
  let should_work input n =
    expect_eq ~raise program "foobar" (e_int ~loc input) (e_int ~loc n)
  in
  let () = should_fail 10 in
  let () = should_fail @@ -10 in
  let () = should_work 5 6 in
  ()
*)

let failwith_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/failwith.mligo" in
  let make_input = e_pair ~loc (e_unit ~loc ()) (e_unit ~loc ()) in
  expect_fail ~raise program "main" make_input


let failwith_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/failwith.jsligo" in
  let make_input = e_pair ~loc (e_unit ~loc ()) (e_unit ~loc ()) in
  expect_fail ~raise program "main" make_input


let assert_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/assert.mligo" in
  let make_input1 b = e_bool ~loc b in
  let input2 = e_unit ~loc () in
  let make_expected =
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_unit ~loc ())
  in
  let _ = expect_fail_twice ~raise program "main" (make_input1 false) input2 in
  let _ = expect_eq_twice ~raise program "main" (make_input1 true) input2 make_expected in
  let _ = expect_fail ~raise program "some" (e_none ~loc ()) in
  let _ =
    expect_eq ~raise program "some" (e_some ~loc (e_unit ~loc ())) (e_unit ~loc ())
  in
  let _ = expect_fail ~raise program "none" (e_some ~loc (e_unit ~loc ())) in
  let _ =
    expect_eq
      ~raise
      program
      "none"
      (e_annotation ~loc (e_none ~loc ()) (t_option ~loc (t_unit ~loc ())))
      (e_unit ~loc ())
  in
  ()


let assert_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/assert.jsligo" in
  let make_input1 b = e_bool ~loc b in
  let input2 = e_unit ~loc () in
  let make_expected =
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_unit ~loc ())
  in
  let _ = expect_fail_twice ~raise program "main" (make_input1 false) input2 in
  let _ = expect_eq_twice ~raise program "main" (make_input1 true) input2 make_expected in
  ()


let recursion_ligo ~raise f : unit =
  let program = type_file ~raise f in
  let _ =
    let make_input = e_pair ~loc (e_int ~loc 10) (e_int ~loc 0) in
    let make_expected = e_int ~loc 55 in
    expect_eq ~raise program "sum" make_input make_expected
  in
  let _ =
    let make_input = e_tuple ~loc [ e_int ~loc 10; e_int ~loc 1; e_int ~loc 1 ] in
    let make_expected = e_int ~loc 89 in
    expect_eq ~raise program "fibo" make_input make_expected
  in
  ()


let guess_string_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/guess_string.mligo" in
  let make_input n = e_pair ~loc (e_int ~loc n) (e_int ~loc 42) in
  let make_expected n =
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (42 + n))
  in
  expect_eq_n ~raise program "main" make_input make_expected


let basic_mligo ~raise () : unit =
  let typed = type_file ~raise "./contracts/basic.mligo" in
  expect_eq_evaluate ~raise typed "foo" (e_int ~loc (42 + 127))


let let_in_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/letin.mligo" in
  let () =
    let make_input n =
      e_pair ~loc (e_int ~loc n) (e_pair ~loc (e_int ~loc 3) (e_int ~loc 5))
    in
    let make_expected n =
      e_pair
        ~loc
        (e_typed_list ~loc [] (t_operation ~loc ()))
        (e_pair ~loc (e_int ~loc (7 + n)) (e_int ~loc (3 + 5)))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () =
    expect_eq ~raise program "letin_nesting" (e_unit ~loc ()) (e_string ~loc "test")
  in
  let () = expect_eq ~raise program "letin_nesting2" (e_int ~loc 4) (e_int ~loc 9) in
  ()


let let_in_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/letin.jsligo" in
  let () =
    let make_input n = e_int ~loc n, e_pair ~loc (e_int ~loc 3) (e_int ~loc 5) in
    let make_expected n =
      e_pair
        ~loc
        (e_typed_list ~loc [] (t_operation ~loc ()))
        (e_pair ~loc (e_int ~loc (7 + n)) (e_int ~loc (3 + 5)))
    in
    expect_eq_n_twice ~raise program "main" make_input make_expected
  in
  let () =
    expect_eq ~raise program "letin_nesting" (e_unit ~loc ()) (e_string ~loc "test")
  in
  let () = expect_eq ~raise program "letin_nesting2" (e_int ~loc 4) (e_int ~loc 9) in
  ()


let local_type_decl ~raise program : unit =
  let () = expect_eq ~raise program "local_type" (e_unit ~loc ()) (e_int ~loc 3) in
  ()


(*
let local_type_decl ~raise () : unit =
  let program = type_file ~raise "./contracts/local_type_decl.ligo" in
  local_type_decl ~raise program
*)

let match_variant ~raise () : unit =
  let program = type_file ~raise "./contracts/match.mligo" in
  let () =
    let make_input n =
      e_pair ~loc (e_constructor ~loc "Sub" (e_int ~loc n)) (e_int ~loc 3)
    in
    let make_expected n =
      e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (3 - n))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () =
    let input = e_bool ~loc true in
    let expected = e_int ~loc 10 in
    expect_eq ~raise program "match_bool" input expected
  in
  let () =
    let input = e_bool ~loc false in
    let expected = e_int ~loc 0 in
    expect_eq ~raise program "match_bool" input expected
  in
  let () =
    let input = e_list ~loc [ e_int ~loc 3 ] in
    let expected = e_int ~loc 3 in
    expect_eq ~raise program "match_list" input expected
  in
  let () =
    let input = e_typed_list ~loc [] (t_int ~loc ()) in
    let expected = e_int ~loc 10 in
    expect_eq ~raise program "match_list" input expected
  in
  let () =
    let make_input n = e_some ~loc (e_int ~loc n) in
    let make_expected n = e_int ~loc n in
    expect_eq_n ~raise program "match_option" make_input make_expected
  in
  ()


let match_variant_js ~raise () : unit =
  let program = type_file ~raise "./contracts/match.jsligo" in
  let make_input n = e_constructor ~loc "Sub" (e_int ~loc n), e_int ~loc 3 in
  let make_expected n =
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (3 - n))
  in
  expect_eq_n_twice ~raise program "main" make_input make_expected


let match_matej ~raise () : unit =
  let program = type_file ~raise "./contracts/match_bis.mligo" in
  let make_input n = e_constructor ~loc "Decrement" (e_int ~loc n), e_int ~loc 3 in
  let make_expected n =
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (3 - n))
  in
  expect_eq_n_twice ~raise program "main" make_input make_expected


let match_matej_js ~raise () : unit =
  let program = type_file ~raise "./contracts/match_bis.jsligo" in
  let make_input n = e_constructor ~loc "Decrement" (e_int ~loc n), e_int ~loc 3 in
  let make_expected n =
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (3 - n))
  in
  expect_eq_n_twice ~raise program "main" make_input make_expected


let mligo_list ~raise () : unit =
  let program = type_file ~raise "./contracts/list.mligo" in
  let () =
    expect_eq
      ~raise
      program
      "size_"
      (e_list ~loc [ e_int ~loc 0; e_int ~loc 1; e_int ~loc 2 ])
      (e_nat ~loc 3)
  in
  let aux lst = e_list ~loc @@ List.map ~f:(e_int ~loc) lst in
  let () = expect_eq ~raise program "fold_op" (aux [ 1; 2; 3 ]) (e_int ~loc 16) in
  let () = expect_eq ~raise program "fold_left" (aux [ 1; 2; 3 ]) (aux [ 3; 2; 1 ]) in
  let () = expect_eq ~raise program "fold_right" (aux [ 1; 2; 3 ]) (aux [ 1; 2; 3 ]) in
  let () =
    let make_input n =
      ( e_list ~loc [ e_int ~loc n; e_int ~loc (2 * n) ]
      , e_pair ~loc (e_int ~loc 3) (e_list ~loc [ e_int ~loc 8 ]) )
    in
    let make_expected n =
      e_pair
        ~loc
        (e_typed_list ~loc [] (t_operation ~loc ()))
        (e_pair ~loc (e_int ~loc (n + 3)) (e_list ~loc [ e_int ~loc (2 * n) ]))
    in
    expect_eq_n_twice ~raise program "main" make_input make_expected
  in
  let () = expect_eq_evaluate ~raise program "x" (e_list ~loc []) in
  let () =
    expect_eq_evaluate
      ~raise
      program
      "y"
      (e_list ~loc @@ List.map ~f:(e_int ~loc) [ 3; 4; 5 ])
  in
  let () =
    expect_eq_evaluate
      ~raise
      program
      "z"
      (e_list ~loc @@ List.map ~f:(e_int ~loc) [ 2; 3; 4; 5 ])
  in
  let () = expect_eq_evaluate ~raise program "find_x" (e_none ~loc ()) in
  let () = expect_eq_evaluate ~raise program "find_y4" (e_some ~loc (e_int ~loc 4)) in
  let () = expect_eq_evaluate ~raise program "find_y6" (e_none ~loc ()) in
  let () = expect_eq_evaluate ~raise program "find_z2" (e_some ~loc (e_int ~loc 2)) in
  let () = expect_eq ~raise program "map_op" (aux [ 2; 3; 4; 5 ]) (aux [ 3; 4; 5; 6 ]) in
  let () = expect_eq ~raise program "iter_op" (aux [ 2; 3; 4; 5 ]) (e_unit ~loc ()) in
  ()


let jsligo_list ~raise () : unit =
  let program = type_file ~raise "./contracts/list.jsligo" in
  let () =
    expect_eq
      ~raise
      program
      "size_"
      (e_list ~loc [ e_int ~loc 0; e_int ~loc 1; e_int ~loc 2 ])
      (e_nat ~loc 3)
  in
  let aux lst = e_list ~loc @@ List.map ~f:(e_int ~loc) lst in
  let () = expect_eq ~raise program "fold_op" (aux [ 1; 2; 3 ]) (e_int ~loc 16) in
  let () =
    let make_input n =
      ( e_list ~loc [ e_int ~loc n; e_int ~loc (2 * n) ]
      , e_pair ~loc (e_int ~loc 3) (e_list ~loc [ e_int ~loc 8 ]) )
    in
    let make_expected n =
      e_pair
        ~loc
        (e_typed_list ~loc [] (t_operation ~loc ()))
        (e_pair ~loc (e_int ~loc (n + 3)) (e_list ~loc [ e_int ~loc (2 * n) ]))
    in
    expect_eq_n_twice ~raise program "main" make_input make_expected
  in
  let () = expect_eq_evaluate ~raise program "x" (e_list ~loc []) in
  let () =
    expect_eq_evaluate
      ~raise
      program
      "y"
      (e_list ~loc @@ List.map ~f:(e_int ~loc) [ 3; 4; 5 ])
  in
  let () =
    expect_eq_evaluate
      ~raise
      program
      "z"
      (e_list ~loc @@ List.map ~f:(e_int ~loc) [ 2; 3; 4; 5 ])
  in
  let () = expect_eq_evaluate ~raise program "find_x" (e_none ~loc ()) in
  let () = expect_eq_evaluate ~raise program "find_y4" (e_some ~loc (e_int ~loc 4)) in
  let () = expect_eq_evaluate ~raise program "find_y6" (e_none ~loc ()) in
  let () = expect_eq_evaluate ~raise program "find_z2" (e_some ~loc (e_int ~loc 2)) in
  let () = expect_eq ~raise program "map_op" (aux [ 2; 3; 4; 5 ]) (aux [ 3; 4; 5; 6 ]) in
  let () = expect_eq ~raise program "iter_op" (aux [ 2; 3; 4; 5 ]) (e_unit ~loc ()) in
  ()


let lambda ~raise f : unit =
  let program = type_file ~raise f in
  let make_expected = e_unit ~loc () in
  expect_eq_twice ~raise program "main" (e_unit ~loc ()) (e_unit ~loc ()) make_expected


let lambda2 ~raise f : unit =
  let program = type_file ~raise f in
  let make_expected = e_unit ~loc () in
  expect_eq_twice ~raise program "main" (e_unit ~loc ()) (e_unit ~loc ()) make_expected


(* Why was this test not used below? -- Christian

let fibo_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/fibo.mligo" in
  let make_input = e_pair ~loc (e_unit ~loc ()) (e_unit ~loc ()) in
  let make_expected = e_int ~loc 42 in
  expect_eq ~raise program "main" make_input make_expected
*)

let michelson_insertion ~raise program : unit =
  let program = program in
  let make_input n = e_pair ~loc (e_nat ~loc n) (e_nat ~loc 1) in
  let make_expected n = e_nat ~loc (n + 1) in
  expect_eq_n_pos ~raise program "michelson_add" make_input make_expected


let michelson_insertion ~raise f : unit = michelson_insertion ~raise @@ type_file ~raise f

(*
let website1_ligo ~raise () : unit =
  let program = type_file ~raise "./contracts/website1.ligo" in
  let make_input n = e_pair ~loc (e_int ~loc n) (e_int ~loc 42) in
  let make_expected _n =
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (42 + 1))
  in
  expect_eq_n ~raise program "main" make_input make_expected
*)

let website2_ligo ~raise f : unit =
  let program = type_file ~raise f in
  let make_input n =
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_constructor ~loc action (e_int ~loc n), e_int ~loc 42
  in
  let make_expected n =
    let op = if n mod 2 = 0 then ( + ) else ( - ) in
    e_pair ~loc (e_typed_list ~loc [] (t_operation ~loc ())) (e_int ~loc (op 42 n))
  in
  expect_eq_n_twice ~raise program "main" make_input make_expected


(*
let tez_ligo ~raise () : unit =
  let program = type_file ~raise "./contracts/tez.ligo" in
  let _ = expect_eq_evaluate ~raise program "add_tez" (e_mutez ~loc 42) in
  let _ = expect_eq_evaluate ~raise program "sub_tez" (e_some ~loc (e_mutez ~loc 1)) in
  let _ = expect_eq_evaluate ~raise program "sub_tez_none" (e_none ~loc ()) in
  let _ =
    expect_eq_evaluate ~raise program "not_enough_tez" (e_mutez ~loc 4611686018427387903)
  in
  let _ = expect_eq_evaluate ~raise program "nat_mul_tez" (e_mutez ~loc 100) in
  let _ = expect_eq_evaluate ~raise program "tez_mul_nat" (e_mutez ~loc 1000) in
  let _ = expect_eq_evaluate ~raise program "tez_div_tez1" (e_nat ~loc 100) in
  let _ = expect_eq_evaluate ~raise program "tez_div_tez2" (e_nat ~loc 1) in
  let _ = expect_eq_evaluate ~raise program "tez_div_tez3" (e_nat ~loc 0) in
  let _ = expect_eq_evaluate ~raise program "tez_mod_tez1" (e_mutez ~loc 0) in
  let _ = expect_eq_evaluate ~raise program "tez_mod_tez2" (e_mutez ~loc 10) in
  let _ = expect_eq_evaluate ~raise program "tez_mod_tez3" (e_mutez ~loc 100) in
  ()
*)

let tez_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/tez.mligo" in
  let _ = expect_eq_evaluate ~raise program "add_tez" (e_mutez ~loc 42) in
  let _ = expect_eq_evaluate ~raise program "sub_tez" (e_some ~loc (e_mutez ~loc 1)) in
  let _ = expect_eq_evaluate ~raise program "sub_tez_none" (e_none ~loc ()) in
  let _ =
    expect_eq_evaluate ~raise program "not_enough_tez" (e_mutez ~loc 4611686018427387903)
  in
  let _ = expect_eq_evaluate ~raise program "add_more_tez" (e_mutez ~loc 111111000) in
  ()


let mligo_let_multiple ~raise () : unit =
  let program = type_file ~raise "./contracts/let_multiple.mligo" in
  let () =
    let input = e_unit ~loc () in
    let expected = e_int ~loc 3 in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_unit ~loc () in
    let expected = e_int ~loc 6 in
    expect_eq ~raise program "main_paren" input expected
  in
  let () =
    let input = e_unit ~loc () in
    let expected = e_tuple ~loc [ e_int ~loc 23; e_int ~loc 42 ] in
    expect_eq ~raise program "correct_values_bound" input expected
  in
  let () =
    let input = e_unit ~loc () in
    let expected = e_int ~loc 19 in
    expect_eq ~raise program "non_tuple_rhs" input expected
  in
  let () =
    let input = e_unit ~loc () in
    let expected =
      e_tuple
        ~loc
        [ e_int ~loc 10; e_int ~loc 20; e_int ~loc 30; e_int ~loc 40; e_int ~loc 50 ]
    in
    expect_eq ~raise program "correct_values_big_tuple" input expected
  in
  let () =
    let input = e_unit ~loc () in
    let expected = e_tuple ~loc [ e_int ~loc 10; e_string ~loc "hello" ] in
    expect_eq ~raise program "correct_values_different_types" input expected
  in
  ()


let jsligo_let_multiple ~raise () : unit =
  let program = type_file ~raise "./contracts/let_multiple.jsligo" in
  let () =
    let input = e_unit ~loc () in
    let expected = e_int ~loc 3 in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_unit ~loc () in
    let expected = e_int ~loc 6 in
    expect_eq ~raise program "main_paren" input expected
  in
  let () =
    let input = e_unit ~loc () in
    let expected = e_int ~loc 65 in
    expect_eq ~raise program "non_tuple_rhs" input expected
  in
  ()


let balance_test_options ~raise () =
  let balance =
    trace_option ~raise (test_internal "could not convert balance")
    @@ Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "0"
  in
  Proto_alpha_utils.Memory_proto_alpha.(
    make_options ~env:(test_environment ()) ~balance ())


let balance_constant ~raise f : unit =
  let program = type_file ~raise f in
  let expected = e_tuple ~loc [ e_list ~loc []; e_mutez ~loc 0 ] in
  let options = balance_test_options ~raise () in
  expect_eq_twice
    ~raise
    ~options
    program
    "main"
    (e_unit ~loc ())
    (e_mutez ~loc 0)
    expected


let amount ~raise f : unit =
  let program = type_file ~raise f in
  let input = e_unit ~loc () in
  let expected = e_int ~loc 42 in
  let amount =
    match Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "100" with
    | Some t -> t
    | None -> Memory_proto_alpha.Protocol.Alpha_context.Tez.one
  in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(
      make_options ~env:(test_environment ()) ~amount ())
  in
  expect_eq ~raise ~options program "check_" input expected


let addr_test ~raise program =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr =
    Protocol.Alpha_context.Contract.to_b58check
    @@ (List.nth_exn (test_environment ()).identities 0).implicit_contract
  in
  let open Tezos_crypto in
  let key_hash =
    Signature.Public_key_hash.to_b58check
    @@ (List.nth_exn (test_environment ()).identities 0).public_key_hash
  in
  expect_eq ~raise program "main" (e_key_hash ~loc key_hash) (e_address ~loc addr)


let address ~raise f : unit =
  let program = type_file ~raise f in
  addr_test ~raise program


let self_address ~raise f : unit =
  let _ = type_file ~raise f in
  ()


let implicit_account ~raise f : unit =
  let _ = type_file ~raise f in
  ()


let tuples_sequences_functions_jsligo ~raise () : unit =
  let _ = type_file ~raise "./contracts/tuples_sequences_functions.jsligo" in
  ()


let is_nat ~raise f : unit =
  let program = type_file ~raise f in
  let () =
    let input = e_int ~loc 10 in
    let expected = e_some ~loc (e_nat ~loc 10) in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_int ~loc (-10) in
    let expected = e_none ~loc () in
    expect_eq ~raise program "main" input expected
  in
  ()


(*
let simple_access_ligo ~raise () : unit =
  let program = type_file ~raise "./contracts/simple_access.ligo" in
  let make_input = e_tuple ~loc [ e_int ~loc 0; e_int ~loc 1 ] in
  let make_expected = e_int ~loc 2 in
  expect_eq ~raise program "main" make_input make_expected

let deep_access_ligo ~raise () : unit =
  let program = type_file ~raise "./contracts/deep_access.ligo" in
  let () =
    let make_input = e_unit ~loc () in
    let make_expected = e_int ~loc 2 in
    expect_eq ~raise program "main" make_input make_expected
  in
  let () =
    let make_input = e_unit ~loc () in
    let make_expected = e_int ~loc 6 in
    expect_eq ~raise program "asymetric_tuple_access" make_input make_expected
  in
  let () =
    let make_input =
      e_record_ez
        ~loc
        [ ( "nesty"
          , e_record_ez
              ~loc
              [ "mymap", e_typed_map ~loc [] (t_int ~loc ()) (t_string ~loc ()) ] )
        ]
    in
    let make_expected = e_string ~loc "one" in
    expect_eq ~raise program "nested_record" make_input make_expected
  in
  ()
*)

let attributes ~raise f : unit =
  let program = type_file ~raise f in
  let () =
    let input = e_int ~loc 3 in
    let expected = e_int ~loc 5 in
    expect_eq ~raise program "foo" input expected
  in
  ()


(*
let get_contract_ligo ~raise () : unit =
  let program = type_file ~raise "./contracts/get_contract.ligo" in
  let () =
    let make_input _n = e_unit ~loc () in
    let make_expected : int -> Ast_core.expression -> unit =
     fun _n result ->
      let ops, storage =
        trace_option ~raise (test_internal __LOC__)
        @@ Ast_core.get_e_pair result.expression_content
      in
      let () =
        let lst =
          trace_option ~raise (test_internal __LOC__)
          @@ Ast_core.get_e_list ops.expression_content
        in
        Assert.assert_list_size ~raise (test_internal __LOC__) lst 1
      in
      let expected_storage = Ast_core.e_unit ~loc () in
      trace_option ~raise (test_internal __LOC__)
      @@ Ast_core.Misc.assert_value_eq (expected_storage, storage)
    in
    let () =
      let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
      let options =
        Proto_alpha_utils.Memory_proto_alpha.(
          make_options ~env:(test_environment ()) ~amount ())
      in
      let () =
        expect_n_strict_pos_small ~raise ~options program "cb" make_input make_expected
      in
      expect_n_strict_pos_small ~raise ~options program "cbo" make_input make_expected
    in
    ()
  in
  ()

let chain_id ~raise () : unit =
  let program = type_file ~raise "./contracts/chain_id.ligo" in
  let pouet =
    Tezos_crypto.Base58.simple_encode
      Tezos_base__TzPervasives.Chain_id.b58check_encoding
      Tezos_base__TzPervasives.Chain_id.zero
  in
  let make_input = e_chain_id ~loc pouet in
  let make_expected = e_chain_id ~loc pouet in
  let () = expect_eq ~raise program "chain_id" make_input make_expected in
  ()
*)

let key_hash ~raise f : unit =
  let open Tezos_crypto in
  let raw_pkh, raw_pk, _ = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let program = type_file ~raise f in
  let make_input = e_pair ~loc (e_key_hash ~loc pkh_str) (e_key ~loc pk_str) in
  let make_expected = e_pair ~loc (e_bool ~loc true) (e_key_hash ~loc pkh_str) in
  let () = expect_eq ~raise program "check_hash_key" make_input make_expected in
  ()


let check_signature ~raise f : unit =
  let open Tezos_crypto in
  let _, raw_pk, sk = Signature.generate_key () in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let signed = Signature.sign sk (Bytes.of_string "hello world") in
  let program = type_file ~raise f in
  let make_input =
    e_tuple
      ~loc
      [ e_key ~loc pk_str
      ; e_signature ~loc (Signature.to_b58check signed)
      ; e_bytes_string ~loc "hello world"
      ]
  in
  let make_expected = e_bool ~loc true in
  let () = expect_eq ~raise program "check_signature" make_input make_expected in
  ()


let curry ~raise () : unit =
  let program = type_file ~raise "./contracts/curry.mligo" in
  let () = expect_eq ~raise program "main" (e_int ~loc 2) (e_int ~loc 12) in
  let () = expect_eq ~raise program "partial_apply" (e_int ~loc 2) (e_int ~loc 12) in
  ()


let set_delegate ~raise f : unit =
  let open Tezos_crypto in
  let raw_pkh, _, _ = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let program = type_file ~raise f in
  let () =
    expect_eq
      ~raise
      program
      "main"
      (e_key_hash ~loc pkh_str)
      (e_typed_list ~loc [] (t_operation ~loc ()))
  in
  ()


let type_tuple_destruct ~raise () : unit =
  let program = type_file ~raise "./contracts/type_tuple_destruct.mligo" in
  let () = expect_eq ~raise program "type_tuple_d" (e_unit ~loc ()) (e_int ~loc 35) in
  let () =
    expect_eq
      ~raise
      program
      "type_tuple_d_2"
      (e_unit ~loc ())
      (e_string ~loc "helloworld")
  in
  ()


let tuple_param_destruct ~raise () : unit =
  let program = type_file ~raise "./contracts/tuple_param_destruct.mligo" in
  let () =
    expect_eq
      ~raise
      program
      "sum"
      (e_tuple ~loc [ e_int ~loc 20; e_int ~loc 10 ])
      (e_int ~loc 10)
  in
  let () =
    expect_eq
      ~raise
      program
      "parentheses"
      (e_tuple ~loc [ e_int ~loc 20; e_int ~loc 10 ])
      (e_int ~loc 10)
  in
  ()


let let_in_multi_bind ~raise () : unit =
  let program = type_file ~raise "./contracts/let_in_multi_bind.mligo" in
  let () =
    expect_eq
      ~raise
      program
      "sum"
      (e_tuple ~loc [ e_int ~loc 10; e_int ~loc 10 ])
      (e_int ~loc 20)
  in
  let () =
    expect_eq
      ~raise
      program
      "sum2"
      (e_tuple
         ~loc
         [ e_string ~loc "my"
         ; e_string ~loc "name"
         ; e_string ~loc "is"
         ; e_string ~loc "bob"
         ])
      (e_string ~loc "mynameisbob")
  in
  ()


let bytes_unpack ~raise f : unit =
  let program = type_file ~raise f in
  let () =
    expect_eq
      ~raise
      program
      "id_string"
      (e_string ~loc "teststring")
      (e_some ~loc (e_string ~loc "teststring"))
  in
  let () =
    expect_eq ~raise program "id_int" (e_int ~loc 42) (e_some ~loc (e_int ~loc 42))
  in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr =
    Protocol.Alpha_context.Contract.to_b58check
    @@ (List.nth_exn (test_environment ()).identities 0).implicit_contract
  in
  let () =
    expect_eq
      ~raise
      program
      "id_address"
      (e_address ~loc addr)
      (e_some ~loc (e_address ~loc addr))
  in
  ()


let empty_case ~raise f : unit =
  let program = type_file ~raise f in
  let () =
    let input _ = e_constructor ~loc "Bar" (e_int ~loc 1) in
    let expected _ = e_int ~loc 1 in
    expect_eq_n ~raise program "main" input expected
  in
  let () =
    let input _ = e_constructor ~loc "Baz" (e_unit ~loc ()) in
    let expected _ = e_int ~loc (-1) in
    expect_eq_n ~raise program "main" input expected
  in
  ()


let tuple_type_mligo ~raise () : unit =
  let program = type_file ~raise "./contracts/tuple_type.mligo" in
  let () =
    let input _ = e_int ~loc 0 in
    let expected _ = e_int ~loc 8 in
    expect_eq_n ~raise program "test1" input expected
  in
  let () =
    let input _ = e_int ~loc 0 in
    let expected _ = e_int ~loc 12 in
    expect_eq_n ~raise program "test2" input expected
  in
  ()


let tuple_type_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/tuple_type.jsligo" in
  let () =
    let input _ = e_int ~loc 0 in
    let expected _ = e_int ~loc 8 in
    expect_eq_n ~raise program "arguments_test" input expected
  in
  let () =
    let input _ = e_int ~loc 0 in
    let expected _ = e_int ~loc 8 in
    expect_eq_n ~raise program "tuple_test" input expected
  in
  let () =
    let input _ = e_int ~loc 0 in
    let expected _ = e_int ~loc 8 in
    expect_eq_n ~raise program "arguments_test_inline" input expected
  in
  let () =
    let input _ = e_int ~loc 0 in
    let expected _ = e_int ~loc 8 in
    expect_eq_n ~raise program "tuple_test_inline" input expected
  in
  ()


let no_semicolon_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/no_semicolon.jsligo" in
  let () =
    let input _ = e_int ~loc 2 in
    let expected _ = e_int ~loc 3 in
    expect_eq_n ~raise program "a" input expected
  in
  ()


let tuple_list_jsligo ~raise () : unit =
  let _ = type_file ~raise "./contracts/tuple_list.jsligo" in
  ()


(*
let loop_bugs_ligo ~raise () : unit =
  let program = type_file ~raise "./contracts/loop_bugs.ligo" in
  let input = e_unit ~loc () in
  let () =
    let expected = e_string ~loc "tata" in
    expect_eq ~raise program "shadowing_in_body" input expected
  in
  let () =
    let expected = e_string ~loc "toto" in
    expect_eq ~raise program "shadowing_assigned_in_body" input expected
  in
  ()
*)

let loop_bugs_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/loop_bugs.jsligo" in
  let input1, input2 = e_unit ~loc (), e_int ~loc 0 in
  let () =
    let expected = e_pair ~loc (e_list ~loc []) (e_int ~loc 1) in
    expect_eq_twice ~raise program "main" input1 input2 expected
  in
  ()


let if_no_else_jsligo ~raise () : unit =
  let _ = type_file ~raise "./contracts/if_no_else.jsligo" in
  ()


let tuple_assignment_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/tuple_assignment.jsligo" in
  expect_eq
    ~raise
    program
    "tuple_assignment"
    (e_unit ~loc ())
    (e_tuple ~loc [ e_int ~loc 2; e_int ~loc 5 ])


(*
let chained_assignment_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/chained_assignment.jsligo" in
  expect_eq ~raise program "bar" (e_unit ~loc ()) (e_int ~loc 9)
*)

let block_scope_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/block_scope.jsligo" in
  let _ = expect_eq ~raise program "test_1" (e_unit ~loc ()) (e_int ~loc 3) in
  let _ = expect_eq ~raise program "test_2" (e_unit ~loc ()) (e_int ~loc 3) in
  let _ = expect_eq ~raise program "test_3" (e_unit ~loc ()) (e_int ~loc 3) in
  let _ = expect_eq ~raise program "test_4" (e_unit ~loc ()) (e_int ~loc 3) in
  let _ = expect_eq ~raise program "test_5" (e_unit ~loc ()) (e_int ~loc 2) in
  let _ = expect_eq ~raise program "test_6" (e_unit ~loc ()) (e_int ~loc 2) in
  ()

(*
let assignment_operators_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/assignment_operators.jsligo" in
  let _ =
    expect_eq
      ~raise
      program
      "addeq"
      (e_unit ~loc ())
      (e_tuple ~loc [ e_int ~loc 11; e_int ~loc 9; e_int ~loc 5 ])
  in
  let _ =
    expect_eq
      ~raise
      program
      "mineq"
      (e_unit ~loc ())
      (e_tuple ~loc [ e_int ~loc 15; e_int ~loc 15; e_int ~loc 1 ])
  in
  let _ =
    expect_eq
      ~raise
      program
      "diveq"
      (e_unit ~loc ())
      (e_tuple ~loc [ e_int ~loc 5; e_int ~loc 4; e_int ~loc 3 ])
  in
  let _ =
    expect_eq
      ~raise
      program
      "multeq"
      (e_unit ~loc ())
      (e_tuple ~loc [ e_int ~loc 2000; e_int ~loc 100; e_int ~loc 12 ])
  in
  let _ =
    expect_eq
      ~raise
      program
      "resteq"
      (e_unit ~loc ())
      (e_tuple ~loc [ e_nat ~loc 2; e_nat ~loc 3; e_nat ~loc 1 ])
  in
  ()
*)

let switch_cases_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/switch_statement.jsligo" in
  let _ =
    expect_eq
      ~raise
      program
      "single_default_return"
      (e_int ~loc 5)
      (e_string ~loc "Hello!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "single_default_no_statements"
      (e_int ~loc 5)
      (e_string ~loc "Hello")
  in
  let _ =
    expect_eq
      ~raise
      program
      "single_default_break_1"
      (e_int ~loc 5)
      (e_string ~loc "HelloWorld")
  in
  let _ =
    expect_eq
      ~raise
      program
      "single_default_break_2"
      (e_int ~loc 5)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "single_case_no_statements"
      (e_int ~loc 1)
      (e_string ~loc "Hello")
  in
  let _ =
    expect_eq
      ~raise
      program
      "single_case_no_statements"
      (e_int ~loc 2)
      (e_string ~loc "Hello")
  in
  let _ =
    expect_eq ~raise program "single_case_return" (e_int ~loc 1) (e_string ~loc "World")
  in
  let _ =
    expect_eq ~raise program "single_case_return" (e_int ~loc 2) (e_string ~loc "Hello")
  in
  let _ =
    expect_eq
      ~raise
      program
      "single_case_fallthrough"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "single_case_fallthrough"
      (e_int ~loc 2)
      (e_string ~loc "Hello ")
  in
  let _ =
    expect_eq
      ~raise
      program
      "single_case_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq ~raise program "single_case_break" (e_int ~loc 2) (e_string ~loc "Hello ")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_fallthrough_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World!!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_fallthrough_break"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_break_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_break_break"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_return_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_return_break"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_fallthrough_return"
      (e_int ~loc 1)
      (e_string ~loc "Hello World!!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_fallthrough_return"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_break_return"
      (e_int ~loc 1)
      (e_string ~loc "Hello World ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_break_return"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_return_return"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_return_return"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_fallthrough"
      (e_int ~loc 1)
      (e_string ~loc "Hello World!!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_fallthrough"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_fallthrough"
      (e_int ~loc 3)
      (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_break"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq ~raise program "case_case_break" (e_int ~loc 3) (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_return"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_return"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_return"
      (e_int ~loc 3)
      (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_fallthrough_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World!!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_fallthrough_break"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_fallthrough_break"
      (e_int ~loc 3)
      (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_break_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_break_break"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_break_break"
      (e_int ~loc 3)
      (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_return_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_return_break"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_return_break"
      (e_int ~loc 3)
      (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_fallthrough_return"
      (e_int ~loc 1)
      (e_string ~loc "Hello World!!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_fallthrough_return"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_fallthrough_return"
      (e_int ~loc 3)
      (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_break_return"
      (e_int ~loc 1)
      (e_string ~loc "Hello World ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_break_return"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_break_return"
      (e_int ~loc 3)
      (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_return_return"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_return_return"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_case_return_return"
      (e_int ~loc 3)
      (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_all_fallthrough"
      (e_int ~loc 1)
      (e_string ~loc "Hello World!!!@@@ ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_all_fallthrough"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!@@@ ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_all_fallthrough"
      (e_int ~loc 3)
      (e_string ~loc "Hello @@@ ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_all_fallthrough"
      (e_int ~loc 4)
      (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_all_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_all_break"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_all_break"
      (e_int ~loc 3)
      (e_string ~loc "Hello @@@ ???")
  in
  let _ =
    expect_eq ~raise program "case_all_break" (e_int ~loc 4) (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_all_return"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq ~raise program "case_all_return" (e_int ~loc 2) (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq ~raise program "case_all_return" (e_int ~loc 3) (e_string ~loc "Hello @@@")
  in
  let _ =
    expect_eq ~raise program "case_all_return" (e_int ~loc 4) (e_string ~loc "Hello  ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_fallthrough"
      (e_int ~loc 1)
      (e_string ~loc "Hello World!!!@@@### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_fallthrough"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!@@@### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_fallthrough"
      (e_int ~loc 3)
      (e_string ~loc "Hello @@@### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_fallthrough"
      (e_int ~loc 4)
      (e_string ~loc "Hello ### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_break"
      (e_int ~loc 1)
      (e_string ~loc "Hello World ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_break"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_break"
      (e_int ~loc 3)
      (e_string ~loc "Hello @@@ ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_break"
      (e_int ~loc 4)
      (e_string ~loc "Hello ### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_return"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_return"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_return"
      (e_int ~loc 3)
      (e_string ~loc "Hello @@@")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_return"
      (e_int ~loc 4)
      (e_string ~loc "Hello ###")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_fallthrough_4"
      (e_int ~loc 1)
      (e_string ~loc "Hello World!!!@@@^^^### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_fallthrough_4"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!@@@^^^### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_fallthrough_4"
      (e_int ~loc 3)
      (e_string ~loc "Hello @@@^^^### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_fallthrough_4"
      (e_int ~loc 4)
      (e_string ~loc "Hello ^^^### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_fallthrough_4"
      (e_int ~loc 5)
      (e_string ~loc "Hello ### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_break_4"
      (e_int ~loc 1)
      (e_string ~loc "Hello World ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_break_4"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!! ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_break_4"
      (e_int ~loc 3)
      (e_string ~loc "Hello @@@ ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_break_4"
      (e_int ~loc 4)
      (e_string ~loc "Hello ^^^ ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_break_4"
      (e_int ~loc 5)
      (e_string ~loc "Hello ### ???")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_return_4"
      (e_int ~loc 1)
      (e_string ~loc "Hello World")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_return_4"
      (e_int ~loc 2)
      (e_string ~loc "Hello !!!")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_return_4"
      (e_int ~loc 3)
      (e_string ~loc "Hello @@@")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_return_4"
      (e_int ~loc 4)
      (e_string ~loc "Hello ^^^")
  in
  let _ =
    expect_eq
      ~raise
      program
      "case_default_all_return_4"
      (e_int ~loc 5)
      (e_string ~loc "Hello ###")
  in
  ()


let ternary_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/ternary.jsligo" in
  let _ = expect_eq ~raise program "foo" (e_int ~loc 1001) (e_int ~loc 1000) in
  let _ = expect_eq ~raise program "foo" (e_int ~loc 101) (e_int ~loc 100) in
  let _ = expect_eq ~raise program "foo" (e_int ~loc 1) (e_int ~loc 0) in
  let _ = expect_eq ~raise program "foo" (e_int ~loc 0) (e_int ~loc 2) in
  ()


let if_if_return_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/if_if_return.jsligo" in
  let _ = expect_eq ~raise program "foo" (e_int ~loc 1001) (e_int ~loc 1000) in
  let _ = expect_eq ~raise program "foo" (e_int ~loc 101) (e_int ~loc 100) in
  let _ = expect_eq ~raise program "foo" (e_int ~loc 1) (e_int ~loc 0) in
  let _ = expect_eq ~raise program "foo" (e_int ~loc 0) (e_int ~loc 2) in
  ()


let while_and_for_loops_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/loops.jsligo" in
  let _ =
    expect_eq
      ~raise
      program
      "for_of_single_statement"
      (e_list ~loc [ e_int ~loc 1; e_int ~loc 2; e_int ~loc 3 ])
      (e_list ~loc [ e_int ~loc 3; e_int ~loc 2; e_int ~loc 1 ])
  in
  let _ =
    expect_eq
      ~raise
      program
      "for_of_multi_statements_1"
      (e_list ~loc [ e_int ~loc 1; e_int ~loc 2; e_int ~loc 3 ])
      (e_list ~loc [ e_int ~loc 5; e_int ~loc 4; e_int ~loc 3 ])
  in
  let _ =
    expect_eq
      ~raise
      program
      "for_of_multi_statements_2"
      (e_list ~loc [ e_int ~loc 1; e_int ~loc 2; e_int ~loc 3 ])
      (e_list ~loc [ e_int ~loc 6; e_int ~loc 4; e_int ~loc 2 ])
  in
  let _ =
    expect_eq ~raise program "while_single_statement" (e_int ~loc 10) (e_int ~loc 10)
  in
  let _ =
    expect_eq ~raise program "while_multi_statements_1" (e_int ~loc 10) (e_int ~loc 55)
  in
  let _ =
    expect_eq ~raise program "while_multi_statements_2" (e_int ~loc 10) (e_int ~loc 55)
  in
  ()


let disc_union_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/disc_union.jsligo" in
  let data1, data2 =
    ( e_constructor ~loc "Increment" (e_record_ez ~loc [ "amount", e_int ~loc 42 ])
    , e_int ~loc 22 )
  in
  let _ = expect_eq_twice ~raise program "main" data1 data2 (e_int ~loc 64) in
  let data1, data2 =
    ( e_constructor ~loc "Decrement" (e_record_ez ~loc [ "amount", e_int ~loc 5 ])
    , e_int ~loc 22 )
  in
  let _ = expect_eq_twice ~raise program "main" data1 data2 (e_int ~loc 17) in
  ()


let func_object_destruct_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/jsligo_destructure_object.jsligo" in
  let data =
    e_record_ez
      ~loc
      [ "bar", e_record_ez ~loc [ "color", e_constructor ~loc "red" (e_unit ~loc ()) ] ]
  in
  let _ = expect_eq ~raise program "x" data (e_int ~loc 1) in
  let data =
    e_record_ez
      ~loc
      [ "bar", e_record_ez ~loc [ "color", e_constructor ~loc "white" (e_unit ~loc ()) ] ]
  in
  let _ = expect_eq ~raise program "x" data (e_int ~loc 2) in
  let data =
    e_record_ez
      ~loc
      [ "bar", e_record_ez ~loc [ "color", e_constructor ~loc "blue" (e_unit ~loc ()) ] ]
  in
  let _ = expect_eq ~raise program "x" data (e_int ~loc 5) in
  ()


let func_tuple_destruct_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/jsligo_destructure_tuples.jsligo" in
  let data =
    e_tuple
      ~loc
      [ e_tuple
          ~loc
          [ e_string ~loc "first"; e_tuple ~loc [ e_int ~loc 1; e_string ~loc "uno" ] ]
      ; e_tuple
          ~loc
          [ e_string ~loc "second"; e_tuple ~loc [ e_int ~loc 2; e_string ~loc "dos" ] ]
      ]
  in
  let _ =
    expect_eq
      ~raise
      program
      "test"
      data
      (e_tuple ~loc [ e_string ~loc "firstsecond"; e_int ~loc 3; e_string ~loc "unodos" ])
  in
  ()


let switch_return_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/switch_return.jsligo" in
  let data =
    e_constructor ~loc "Increment" (e_record_ez ~loc [ "amount", e_int ~loc 42 ])
  in
  let _ = expect_eq ~raise program "main" data (e_int ~loc 51) in
  let data =
    e_constructor ~loc "Decrement" (e_record_ez ~loc [ "amount", e_int ~loc 5 ])
  in
  let _ = expect_eq ~raise program "main" data (e_int ~loc 2) in
  let data =
    e_constructor ~loc "Decrement" (e_record_ez ~loc [ "amount", e_int ~loc 3 ])
  in
  let _ = expect_eq ~raise program "main" data (e_int ~loc 5) in
  let data = e_constructor ~loc "Reset" (e_unit ~loc ()) in
  let _ = expect_eq ~raise program "main" data (e_int ~loc 3) in
  let _ = expect_eq ~raise program "main2" (e_int ~loc 0) (e_int ~loc 11) in
  let _ = expect_eq ~raise program "main2" (e_int ~loc 1) (e_int ~loc 5) in
  let _ = expect_eq ~raise program "main2" (e_int ~loc 2) (e_int ~loc 3) in
  let _ = expect_eq ~raise program "main2" (e_int ~loc 3) (e_int ~loc (-1)) in
  ()

(*
let transitive_jsligo ~raise () : unit =
  let program = type_file ~raise "./contracts/transitive.jsligo" in
  (* let data = e_constructor ~loc "Increment" (e_record ~loc_ez [("amount" , e_int ~loc 42)]) in *)
  let _ =
    expect_eq
      ~raise
      program
      "var"
      (e_unit ~loc ())
      (e_tuple ~loc [ e_int ~loc 2; e_int ~loc 2; e_int ~loc 2 ])
  in
  let _ =
    expect_eq
      ~raise
      program
      "tuple"
      (e_unit ~loc ())
      (e_tuple ~loc [ e_int ~loc 7; e_int ~loc 0; e_int ~loc 7 ])
  in
  ()
*)

let if_semi_jsligo ~raise () : unit =
  let _ = type_file ~raise "./contracts/if_semi.jsligo" in
  ()


let return_handling ~raise () : unit =
  let _ = type_file ~raise "./contracts/return_handling.sligo" in
  ()


let main =
  test_suite "Integration (End to End)"
  @@ [ (* Tezos stuff *)
       (* test_w "chain id" chain_id *) ]
  @ test_w_all "bytes unpack" bytes_unpack
  @ test_w_all "key hash" key_hash
  @ test_w_all "check signature" check_signature
  @ test_w_all "crypto" crypto
  @ test_w_all "balance constant" balance_constant
  @ test_w_all "amount" amount
  @ test_w_all "address" address
  @ test_w_all "self address" self_address
  @ test_w_all "implicit account" implicit_account
  @ [ (* test_w "get_contract (ligo)" get_contract_ligo *)
      (*    ; test_w "tez (ligo)" tez_ligo *)
      test_w "tez (mligo)" tez_mligo
    ]
  (* Ligo stuff *)
  @ test_w_all "lambda" lambda
  @ test_w_all "lambda2" lambda2
  @ [ (* test_w "fibo (mligo)" fibo_mligo ; *)
      (* test_w "fibo2 (mligo)" fibo2_mligo ; *)
      (* test_w "fibo3 (mligo)" fibo3_mligo ; *)
      (* test_w "fibo4 (mligo)" fibo4_mligo ; *) ]
  @ test_w_all "tuple" tuple
  @ [ test_w "tuple type (mligo)" tuple_type_mligo
    ; test_w "tuple type (jsligo)" tuple_type_jsligo
    ]
  @ test_w_all "record" record
  @ test_w_all "option" option
  @ test_w_all "map" map
  @ test_w_all "big_map" big_map
  @ test_w_all "condition" condition
  @ [ test_w "sequence (mligo)" sequence_mligo
      (*    ; test_w "type alias" type_alias *)
      (*    ; test_w "function" function_ *)
      (*    ; test_w "blockless function" blockless *)
      (*    ; test_w "assign" assign *)
      (*    ; test_w "declaration local" declaration_local *)
      (*    ; test_w "complex function" complex_function *)
      (*    ; test_w "anon function" anon_function *)
      (*    ; test_w "various applications" application *)
      (*    ; test_w "closure" closure *)
    ; test_w "closure (mligo)" closure_mligo
    ; test_w "closure (jsligo)" closure_jsligo
    ]
  @ test_w_all "shared-function" shared_function
  @ test_w_all "high-order" higher_order
  @ test_w_all "variant" variant
  @ [ (* test_w "matching" matching *)
      (*    ; test_w "variant matching" variant_matching *)
      test_w "match variant (mligo)" match_variant
    ; test_w "match variant (jsligo)" match_variant_js
    ; test_w "match variant 2 (mligo)" match_matej
    ; test_w "match variant 2 (jsligo)" match_matej_js
    ; test_w "list matching (mligo)" mligo_list
    ; test_w "list matching (jsligo)" jsligo_list
      (* ; test_w "failwith ligo" failwith_ligo *)
    ; test_w "failwith jsligo" failwith_jsligo
    ; test_w "failwith mligo" failwith_mligo
    ; test_w "assert mligo" assert_mligo
    ; test_w "assert jsligo" assert_jsligo
    ]
  @ test_w_all "eq_bool" eq_bool
  @ [ (*      test_w "shadow" shadow *)
      test_w "shadowing (mligo)" shadowing (*    ; test_w "annotation" annotation *)
    ]
  @ test_w_all "multiple-parameters" multiple_parameters
  @ test_w_all "boolean_operators" bool_expression
  @ test_w_all "arithmetic" arithmetic
  @ test_w_all "bitwise_arithmetic" bitwise_arithmetic
  @ test_w_all "string_arithmetic" string_arithmetic
  @ test_w_all "bytes_arithmetic" bytes_arithmetic
  @ test_w_all "set_arithmetic" set_arithmetic
  @ [ test_w "comparable (mligo)" comparable_mligo
      (* ; test_w "unit" unit_expression *)
      (* ; test_w "string" string_expression *)
      (* ; test_w "list" list *)
      (* ; test_w "loop1" loop1 *)
      (* ; test_w "loop2" loop2 *)
      (* ; test_w "loop3" loop3 *)
      (* ; test_w "loop4" loop4 *)
      (* ; test_w "loop5" loop5 *)
      (* ; test_w "loop6" loop6 *)
      (* ; test_w "loop7" loop7 *)
      (* ; test_w "loop8" loop8 *)
      (* ; test_w "loop9" loop9 *)
      (* ; test_w "loop10" loop10 *)
      (* ; test_w "loop11" loop11 *)
      (* ; test_w "loop12" loop12 *)
      (* ; test_w "loop13" loop13 *)
      (* ; test_w "loop14" loop14 *)
      (* ; test_w "loop15" loop15 *)
      (* ; test_w "loop16" loop16 *)
      (* ; test_w "loop17" loop17 *)
      (* ; test_w "loop18" loop18 *)
      (* ; test_w "loop19" loop19 *)
      (* ; test_w "nested_for_loop" nested_for_loop *)
      (* ; test_w "loop" loop *)
    ; test_w "loop (mligo)" loop_mligo
    ; test_w "loop (jsligo)" loop_jsligo
    ; test_w "loop2 (jsligo)" loop2_jsligo
    ]
  @ test_w_all "includer" include_
  @ test_w_all "counter" counter_contract
  @ test_w_all "super-counter" super_counter_contract
  @ [ (* test_w "dispatch counter contract" dispatch_counter_contract *)
      test_w "basic (mligo)" basic_mligo
    ; test_w "let-in (mligo)" let_in_mligo
    ; test_w "let-in (jsligo)" let_in_jsligo
    ; test_w "let multiple (mligo)" mligo_let_multiple
    ; test_w "let multiple (jsligo)" jsligo_let_multiple
      (* ; test_w "local_type_decl" local_type_decl *)
    ]
  @ test_w_all "recursion" recursion_ligo
  (* t_west "guess string mligo" guess_string_mligo ; WIP? *)
  @ test_w_all "michelson_insertion" michelson_insertion
  (*  @ [ test_w "website1 ligo" website1_ligo ] *)
  @ test_w_all "website2" website2_ligo
  @ test_w_all "set delegate" set_delegate
  @ test_w_all "is_nat" is_nat
  @ [ test_w "tuples_sequences_functions (jsligo)" tuples_sequences_functions_jsligo
      (* ; test_w "simple_access (ligo)" simple_access_ligo *)
      (* ; test_w "deep_access (ligo)" deep_access_ligo *)
    ; test_w "curry (mligo)" curry
    ; test_w "type tuple destruct (mligo)" type_tuple_destruct
    ]
  @ test_w_all "attributes" attributes
  @ test_w_all "empty case" empty_case
  @ [ test_w "let in multi-bind (mligo)" let_in_multi_bind
    ; test_w "tuple param destruct (mligo)" tuple_param_destruct
      (* ; test_w "loop_bugs (ligo)" loop_bugs_ligo *)
    ; test_w "loop_bugs (jsligo)" loop_bugs_jsligo
    ; test_w "if no else (jsligo)" if_no_else_jsligo
    ; test_w "tuple_assignment (jsligo)" tuple_assignment_jsligo
    (*    ; test_w "chained_assignment (jsligo)" chained_assignment_jsligo*)
    ; test_w "block_scope (jsligo)" block_scope_jsligo
(*    ; test_w "assignment_operators (jsligo)" assignment_operators_jsligo*)
    ; test_w "if_if_return (jsligo)" if_if_return_jsligo
    ; test_w "switch case (jsligo)" switch_cases_jsligo
    ; test_w "for-of & while loop (jsligo)" while_and_for_loops_jsligo
    ; test_w "discriminated_union (jsligo)" disc_union_jsligo
    ; test_w "ternary (jsligo)" ternary_jsligo
    ; test_w "destruct func object param (jsligo)" func_object_destruct_jsligo
    ; test_w "destruct func tuple param (jsligo)" func_tuple_destruct_jsligo
    ; test_w "switch_return (jsligo)" switch_return_jsligo
    (*    ; test_w "transitive (jsligo)" transitive_jsligo*)
    ; test_w "if_semi (jsligo)" if_semi_jsligo
    ; test_w "return_handling (jsligo)" if_semi_jsligo
    ]
