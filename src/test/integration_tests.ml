open Trace
open Test_helpers
open Main_errors

open Ast_imperative.Combinators

let init_env = Environment.default Environment.Protocols.current
let retype_file f =
  Ligo.Compile.Utils.type_file ~options f "reasonligo" Env
let mtype_file f =
  Ligo.Compile.Utils.type_file ~options f "cameligo" Env
let type_file f =
  Ligo.Compile.Utils.type_file ~options f "pascaligo" Env

let type_alias () : (unit,_) result =
  let%bind program = type_file "./contracts/type-alias.ligo" in
  expect_eq_evaluate program "foo" (e_int 23)

let function_ () : (unit,_) result =
  let%bind program = type_file "./contracts/function.ligo" in
  let make_expect = fun n -> n in
  expect_eq_n_int program "main" make_expect

let blockless () : (unit,_) result =
  let%bind program = type_file "./contracts/blockless.ligo" in
  let make_expect = fun n-> n + 10 in
  expect_eq_n_int program "blockless" make_expect

(* Procedures are not supported yet
  let procedure () : unit result =
  let%bind program = type_file "./contracts/procedure.ligo" in
  let make_expect = fun n -> n + 1 in
  expect_eq_n_int program "main" make_expect *)

let assign () : (unit,_) result =
  let%bind program = type_file "./contracts/assign.ligo" in
  let make_expect = fun n -> n + 1 in
  expect_eq_n_int program "main" make_expect

let annotation () : (unit,_) result =
  let%bind program = type_file "./contracts/annotation.ligo" in
  let%bind () =
    expect_eq_evaluate program "lst" (e_list [])
  in
  let%bind () =
    expect_eq_evaluate program "my_address" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
  in
  ok ()

let complex_function () : (unit,_) result =
  let%bind program = type_file "./contracts/function-complex.ligo" in
  let make_expect = fun n -> (3 * n + 2) in
  expect_eq_n_int program "main" make_expect

let anon_function () : (unit, _) result =
  let%bind program = type_file "./contracts/function-anon.ligo" in
  let%bind () =
    expect_eq_evaluate program "x" (e_int 42)
  in
  ok ()

let application () : (unit, _) result =
  let%bind program = type_file "./contracts/application.ligo" in
  let%bind () =
    let expected = e_int 42 in
    expect_eq_evaluate program "x" expected in
  let%bind () =
    let expected = e_int 42 in
    expect_eq_evaluate program "y" expected in
  let%bind () =
    let expected = e_int 42 in
    expect_eq_evaluate program "z" expected in
  ok ()

let variant () : (unit, _) result =
  let%bind program = type_file "./contracts/variant.ligo" in
  let%bind () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate program "foo" expected in
  let%bind () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate program "bar" expected in
  let%bind () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate program "kee" expected in
  ok ()

let variant_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/variant.mligo" in
  let%bind () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate program "foo" expected in
  let%bind () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate program "bar" expected in
  let%bind () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate program "kee" expected in
  ok ()

let variant_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/variant.religo" in
  let%bind () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate program "foo" expected in
  let%bind () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate program "bar" expected in
  let%bind () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate program "kee" expected in
  ok ()


let variant_matching () : (unit, _) result =
  let%bind program = type_file "./contracts/variant-matching.ligo" in
  let%bind () =
    let make_input = fun n -> e_constructor "Foo" (e_int n) in
    let make_expected = e_int in
    expect_eq program "fb" (make_input 0) (make_expected 0) >>? fun () ->
    expect_eq_n program "fb" make_input make_expected >>? fun () ->
    expect_eq program "fb" (e_constructor "Kee" (e_nat 50)) (e_int 23) >>? fun () ->
    expect_eq program "fb" (e_constructor "Bar" (e_bool true)) (e_int 42) >>? fun () ->
    ok ()
  in
  ok ()

let closure () : (unit, _) result =
  let%bind program = type_file "./contracts/closure.ligo" in
  let%bind program_1 = type_file "./contracts/closure-1.ligo" in
  let%bind program_2 = type_file "./contracts/closure-2.ligo" in
  let%bind program_3 = type_file "./contracts/closure-3.ligo" in
  let%bind _ =
    let make_expect = fun n -> (49 + n) in
    expect_eq_n_int program_3 "foobar" make_expect
  in
  let%bind _ =
    let make_expect = fun n -> (45 + n) in
    expect_eq_n_int program_2 "foobar" make_expect
  in
  let%bind () =
    let make_expect = fun n -> (2 * n) in
    expect_eq_n_int program_1 "foo" make_expect
  in
  let%bind _ =
    let make_expect = fun n -> (4 * n) in
    expect_eq_n_int program "toto" make_expect
  in
  ok ()

let closure_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/closure.mligo" in
  let%bind _ =
    let input = e_int 0 in
    let expected = e_int 25 in
    expect_eq program "test" input expected
  in
  ok ()

let closure_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/closure.religo" in
  let%bind _ =
    let input = e_int 0 in
    let expected = e_int 25 in
    expect_eq program "test" input expected
  in
  ok ()


let shadow () : (unit, _) result =
  let%bind program = type_file "./contracts/shadow.ligo" in
  let make_expect = fun _ -> 0 in
  expect_eq_n_int program "foo" make_expect

let higher_order () : (unit, _) result =
  let%bind program = type_file "./contracts/high-order.ligo" in
  let make_expect = fun n -> n in
  let%bind _ = expect_eq_n_int program "foobar" make_expect in
  let%bind _ = expect_eq_n_int program "foobar2" make_expect in
  let%bind _ = expect_eq_n_int program "foobar3" make_expect in
  let%bind _ = expect_eq_n_int program "foobar4" make_expect in
  let%bind _ = expect_eq_n_int program "foobar5" make_expect in
  (* let%bind _ = applies_expect_eq_n_int program "foobar5" make_expect in *)
  ok ()

let higher_order_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/high-order.mligo" in
  let make_expect = fun n -> n in
  let%bind _ = expect_eq_n_int program "foobar" make_expect in
  let%bind _ = expect_eq_n_int program "foobar2" make_expect in
  let%bind _ = expect_eq_n_int program "foobar3" make_expect in
  let%bind _ = expect_eq_n_int program "foobar4" make_expect in
  let%bind _ = expect_eq_n_int program "foobar5" make_expect in
  ok ()

let higher_order_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/high-order.religo" in
  let make_expect = fun n -> n in
  let%bind _ = expect_eq_n_int program "foobar" make_expect in
  let%bind _ = expect_eq_n_int program "foobar2" make_expect in
  let%bind _ = expect_eq_n_int program "foobar3" make_expect in
  let%bind _ = expect_eq_n_int program "foobar4" make_expect in
  let%bind _ = expect_eq_n_int program "foobar5" make_expect in
  ok ()

let shared_function () : (unit, _) result =
  let%bind program = type_file "./contracts/function-shared.ligo" in
  let%bind () =
    let make_expect = fun n -> (n + 1) in
    expect_eq_n_int program "inc" make_expect
  in
  let%bind () =
    expect_eq program "double_inc" (e_int 0) (e_int 2)
  in
  let%bind () =
    let make_expect = fun n -> (n + 2) in
    expect_eq_n_int program "double_inc" make_expect
  in
  let%bind () =
    let make_expect = fun n -> (2 * n + 3) in
    expect_eq program "foo" (e_int 0) (e_int @@ make_expect 0)
  in
  let%bind () =
    let make_expect = fun n -> (2 * n + 3) in
    expect_eq_n_int program "foo" make_expect
  in
  ok ()

let shared_function_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/function-shared.mligo" in
  let%bind () =
    let make_expect = fun n -> (2 * n + 70) in
    expect_eq_n_int program "foobar" make_expect
  in
  ok ()

let shared_function_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/function-shared.religo" in
  let%bind () =
    let make_expect = fun n -> (2 * n + 70) in
    expect_eq_n_int program "foobar" make_expect
  in
  ok ()

let bool_expression () : (unit, _) result =
  let%bind program = type_file "./contracts/boolean_operators.ligo" in
  let%bind _ =
    let aux (name , f) = expect_eq_b_bool program name f in
    bind_map_list aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
      ("not_bool", fun b -> not b) ;
    ] in
  ok ()

let bool_expression_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/boolean_operators.mligo" in
  let%bind _ =
    let aux (name, f) = expect_eq_b_bool program name f in
    bind_map_list aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
      ("not_bool", fun b -> not b) ;
    ] in
  ok ()

let bool_expression_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/boolean_operators.religo" in
  let%bind _ =
    let aux (name, f) = expect_eq_b_bool program name f in
    bind_map_list aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
      ("not_bool", fun b -> not b) ;
    ] in
  ok ()

let arithmetic () : (unit, _) result =
  let%bind program = type_file "./contracts/arithmetic.ligo" in
  let%bind _ =
    let aux (name , f) = expect_eq_n_int program name f in
    bind_map_list aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
      ("neg_op", fun n -> (-n)) ;
    ] in
  let%bind () = expect_eq_n_pos program "int_op" e_nat e_int in
  let%bind () = expect_eq_n_pos program "mod_op" e_int (fun n -> e_nat (n mod 42)) in
  let%bind () = expect_eq_n_pos program "div_op" e_int (fun n -> e_int (n / 2)) in
  let%bind () = expect_eq_n_pos program "ediv_op" e_int (fun n -> e_some (e_pair (e_int (n/2)) (e_nat (n mod 2)))) in
  ok ()

let arithmetic_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/arithmetic.mligo" in
  let%bind _ =
    let aux (name, f) = expect_eq_n_int program name f in
    bind_map_list aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
      ("neg_op", fun n -> (-n)) ;
      ("neg_op_2", fun n -> -(n + 10)) ;
    ] in
  let%bind () = expect_eq_n_pos program "mod_op" e_int (fun n -> e_nat (n mod 42)) in
  let%bind () = expect_eq_n_pos program "div_op" e_int (fun n -> e_int (n / 2)) in
  let%bind () = expect_eq_n_pos program "ediv_op" e_int (fun n -> e_some (e_pair (e_int (n/2)) (e_nat (n mod 2)))) in
  ok ()

let arithmetic_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/arithmetic.religo" in
  let%bind _ =
    let aux (name, f) = expect_eq_n_int program name f in
    bind_map_list aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
      ("neg_op", fun n -> (-n)) ;
      ("neg_op_2", fun n -> -(n + 10)) ;
    ] in
  let%bind () = expect_eq_n_pos program "mod_op" e_int (fun n -> e_nat (n mod 42)) in
  let%bind () = expect_eq_n_pos program "div_op" e_int (fun n -> e_int (n / 2)) in
  let%bind () = expect_eq_n_pos program "ediv_op" e_int (fun n -> e_some (e_pair (e_int (n/2)) (e_nat (n mod 2)))) in
  ok ()

let bitwise_arithmetic () : (unit, _) result =
  let%bind program = type_file "./contracts/bitwise_arithmetic.ligo" in
  let%bind () = expect_eq program "or_op" (e_nat 7) (e_nat 7) in
  let%bind () = expect_eq program "or_op" (e_nat 3) (e_nat 7) in
  let%bind () = expect_eq program "or_op" (e_nat 2) (e_nat 6) in
  let%bind () = expect_eq program "or_op" (e_nat 14) (e_nat 14) in
  let%bind () = expect_eq program "or_op" (e_nat 10) (e_nat 14) in
  let%bind () = expect_eq program "and_op" (e_nat 7) (e_nat 7) in
  let%bind () = expect_eq program "and_op" (e_nat 3) (e_nat 3) in
  let%bind () = expect_eq program "and_op" (e_nat 2) (e_nat 2) in
  let%bind () = expect_eq program "and_op" (e_nat 14) (e_nat 6) in
  let%bind () = expect_eq program "and_op" (e_nat 10) (e_nat 2) in
  let%bind () = expect_eq program "xor_op" (e_nat 0) (e_nat 7) in
  let%bind () = expect_eq program "xor_op" (e_nat 7) (e_nat 0) in
  let%bind () = expect_eq program "lsl_op" (e_nat 1000) (e_nat 128000) in
  let%bind () = expect_eq program "lsr_op" (e_nat 128000) (e_nat 1000) in
  ok ()

let bitwise_arithmetic_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/bitwise_arithmetic.mligo" in
  let%bind () = expect_eq program "or_op" (e_nat 7) (e_nat 7) in
  let%bind () = expect_eq program "or_op" (e_nat 3) (e_nat 7) in
  let%bind () = expect_eq program "or_op" (e_nat 2) (e_nat 6) in
  let%bind () = expect_eq program "or_op" (e_nat 14) (e_nat 14) in
  let%bind () = expect_eq program "or_op" (e_nat 10) (e_nat 14) in
  let%bind () = expect_eq program "and_op" (e_nat 7) (e_nat 7) in
  let%bind () = expect_eq program "and_op" (e_nat 3) (e_nat 3) in
  let%bind () = expect_eq program "and_op" (e_nat 2) (e_nat 2) in
  let%bind () = expect_eq program "and_op" (e_nat 14) (e_nat 6) in
  let%bind () = expect_eq program "and_op" (e_nat 10) (e_nat 2) in
  let%bind () = expect_eq program "xor_op" (e_nat 0) (e_nat 7) in
  let%bind () = expect_eq program "xor_op" (e_nat 7) (e_nat 0) in
  let%bind () = expect_eq program "lsl_op" (e_nat 1000) (e_nat 128000) in
  let%bind () = expect_eq program "lsr_op" (e_nat 128000) (e_nat 1000) in
  ok ()

let bitwise_arithmetic_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/bitwise_arithmetic.religo" in
  let%bind () = expect_eq program "or_op" (e_nat 7) (e_nat 7) in
  let%bind () = expect_eq program "or_op" (e_nat 3) (e_nat 7) in
  let%bind () = expect_eq program "or_op" (e_nat 2) (e_nat 6) in
  let%bind () = expect_eq program "or_op" (e_nat 14) (e_nat 14) in
  let%bind () = expect_eq program "or_op" (e_nat 10) (e_nat 14) in
  let%bind () = expect_eq program "and_op" (e_nat 7) (e_nat 7) in
  let%bind () = expect_eq program "and_op" (e_nat 3) (e_nat 3) in
  let%bind () = expect_eq program "and_op" (e_nat 2) (e_nat 2) in
  let%bind () = expect_eq program "and_op" (e_nat 14) (e_nat 6) in
  let%bind () = expect_eq program "and_op" (e_nat 10) (e_nat 2) in
  let%bind () = expect_eq program "xor_op" (e_nat 0) (e_nat 7) in
  let%bind () = expect_eq program "xor_op" (e_nat 7) (e_nat 0) in
  let%bind () = expect_eq program "lsl_op" (e_nat 1000) (e_nat 128000) in
  let%bind () = expect_eq program "lsr_op" (e_nat 128000) (e_nat 1000) in
  ok ()

let string_arithmetic () : (unit, _) result =
  let%bind program = type_file "./contracts/string_arithmetic.ligo" in
  let%bind () = expect_eq program "concat_op" (e_string "foo") (e_string "foototo") in
  let%bind () = expect_eq program "concat_op" (e_string "") (e_string "toto") in
  let%bind () = expect_eq program "slice_op" (e_string "tata") (e_string "at") in
  let%bind () = expect_eq program "slice_op" (e_string "foo") (e_string "oo") in
  let%bind () = expect_fail program "slice_op" (e_string "ba") in
  ok ()

let string_arithmetic_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/string_arithmetic.mligo" in
  let%bind () = expect_eq program "size_op"  (e_string "tata") (e_nat 4) in
  let%bind () = expect_eq program "slice_op" (e_string "tata") (e_string "at") in
  let%bind () = expect_eq program "slice_op" (e_string "foo") (e_string "oo") in
  let%bind () = expect_eq program "concat_syntax" (e_string "string_") (e_string "string_test_literal")
  in ok ()

let string_arithmetic_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/string_arithmetic.religo" in
  let%bind () = expect_eq program "size_op"  (e_string "tata") (e_nat 4) in
  let%bind () = expect_eq program "slice_op" (e_string "tata") (e_string "at") in
  let%bind () = expect_eq program "slice_op" (e_string "foo") (e_string "oo") in
  let%bind () = expect_eq program "concat_syntax" (e_string "string_") (e_string "string_test_literal")
  in ok ()


let bytes_arithmetic () : (unit, _) result =
  let%bind program = type_file "./contracts/bytes_arithmetic.ligo" in
  let%bind foo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let%bind foototo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let%bind toto = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "7070" in
  let%bind empty = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "" in
  let%bind tata = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "ff7a7aff" in
  let%bind at = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "7a7a" in
  let%bind ba = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "ba" in
  let%bind () = expect_eq program "concat_op" foo foototo in
  let%bind () = expect_eq program "concat_op" empty toto in
  let%bind () = expect_eq program "slice_op" tata at in
  let%bind () = expect_fail program "slice_op" foo in
  let%bind () = expect_fail program "slice_op" ba in
  let%bind b1 = Test_helpers.run_typed_program_with_imperative_input program "hasherman" foo in
  let%bind () = expect_eq_core program "hasherman" foo b1 in
  let%bind b3 = Test_helpers.run_typed_program_with_imperative_input program "hasherman" foototo in
  let%bind () = trace_assert_fail_option (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (b3 , b1) in
  ok ()

let comparable_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/comparable.mligo" in
  let%bind () = expect_eq program "int_" (e_int 1) (e_bool false) in
  let%bind () = expect_eq program "nat_" (e_nat 1) (e_bool false) in
  let%bind () = expect_eq program "bool_" (e_bool true) (e_bool false) in
  let%bind () = expect_eq program "mutez_" (e_mutez 1) (e_bool false) in
  let%bind () = expect_eq program "string_" (e_string "foo") (e_bool false) in
  let%bind () = expect_eq program "bytes_" (e_bytes_string "deadbeaf") (e_bool false) in
  let%bind () = expect_eq program "address_" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx") (e_bool false) in
  let%bind () = expect_eq program "timestamp_" (e_timestamp 101112) (e_bool false) in
  let open Tezos_crypto in
  let pkh, _, _ = Signature.generate_key () in
  let key_hash = Signature.Public_key_hash.to_b58check @@ pkh in
  let%bind () = expect_eq program "key_hash_" (e_key_hash key_hash) (e_bool false) in
  let pair = e_pair (e_int 1) (e_int 2) in
  let%bind () = expect_eq program "comp_pair" pair (e_bool false) in
  (* let tuple = e_tuple [e_int 1; e_int 2; e_int 3] in
  let%bind () = expect_string_failwith program "uncomp_pair_1" tuple "" in
  let pair = e_pair pair (e_int 3) in
  let%bind () = expect_string_failwith program "uncomp_pair_2" pair "" in *)
  let comb = e_pair (e_int 3) (e_pair (e_int 1) (e_nat 2)) in
  let%bind () = expect_eq program "comb_record" comb (e_bool false) in
  ok ()

let crypto () : (unit, _) result =
  let%bind program = type_file "./contracts/crypto.ligo" in
  let%bind foo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let%bind foototo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let%bind b1 = Test_helpers.run_typed_program_with_imperative_input program "hasherman512" foo in
  let%bind () = expect_eq_core program "hasherman512" foo b1 in
  let%bind b2 = Test_helpers.run_typed_program_with_imperative_input program "hasherman512" foototo in
  let%bind () = trace_assert_fail_option (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b2 , b1) in
  let%bind b4 = Test_helpers.run_typed_program_with_imperative_input program "hasherman_blake" foo in
  let%bind () = expect_eq_core program "hasherman_blake" foo b4 in
  let%bind b5 = Test_helpers.run_typed_program_with_imperative_input program "hasherman_blake" foototo in
  let%bind () = trace_assert_fail_option (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b5 , b4) in
  ok ()

let crypto_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/crypto.mligo" in
  let%bind foo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let%bind foototo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let%bind b1 = Test_helpers.run_typed_program_with_imperative_input program "hasherman512" foo in
  let%bind () = expect_eq_core program "hasherman512" foo b1 in
  let%bind b2 = Test_helpers.run_typed_program_with_imperative_input program "hasherman512" foototo in
  let%bind () = trace_assert_fail_option (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b2 , b1) in
  let%bind b4 = Test_helpers.run_typed_program_with_imperative_input program "hasherman_blake" foo in
  let%bind () = expect_eq_core program "hasherman_blake" foo b4 in
  let%bind b5 = Test_helpers.run_typed_program_with_imperative_input program "hasherman_blake" foototo in
  let%bind () = trace_assert_fail_option (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b5 , b4) in
  ok ()

let crypto_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/crypto.religo" in
  let%bind foo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let%bind foototo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let%bind b1 = Test_helpers.run_typed_program_with_imperative_input program "hasherman512" foo in
  let%bind () = expect_eq_core program "hasherman512" foo b1 in
  let%bind b2 = Test_helpers.run_typed_program_with_imperative_input program "hasherman512" foototo in
  let%bind () = trace_assert_fail_option (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (b2 , b1) in
  let%bind b4 = Test_helpers.run_typed_program_with_imperative_input program "hasherman_blake" foo in
  let%bind () = expect_eq_core program "hasherman_blake" foo b4 in
  let%bind b5 = Test_helpers.run_typed_program_with_imperative_input program "hasherman_blake" foototo in
  let%bind () = trace_assert_fail_option (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (b5 , b4) in
  ok ()

let bytes_arithmetic_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/bytes_arithmetic.mligo" in
  let%bind foo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let%bind foototo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let%bind toto = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "7070" in
  let%bind empty = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "" in
  let%bind tata = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "ff7a7aff" in
  let%bind at = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "7a7a" in
  let%bind ba = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "ba" in
  let%bind () = expect_eq program "concat_op" foo foototo in
  let%bind () = expect_eq program "concat_op" empty toto in
  let%bind () = expect_eq program "slice_op" tata at in
  let%bind () = expect_fail program "slice_op" foo in
  let%bind () = expect_fail program "slice_op" ba in
  let%bind b1 = Test_helpers.run_typed_program_with_imperative_input program "hasherman" foo in
  let%bind () = expect_eq_core program "hasherman" foo b1 in
  let%bind b3 = Test_helpers.run_typed_program_with_imperative_input program "hasherman" foototo in
  let%bind () = trace_assert_fail_option (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b3 , b1) in
  ok ()

let bytes_arithmetic_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/bytes_arithmetic.religo" in
  let%bind foo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let%bind foototo = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let%bind toto = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "7070" in
  let%bind empty = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "" in
  let%bind tata = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "ff7a7aff" in
  let%bind at = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "7a7a" in
  let%bind ba = trace_option (test_internal __LOC__) @@ e_bytes_hex_ez "ba" in
  let%bind () = expect_eq program "concat_op" foo foototo in
  let%bind () = expect_eq program "concat_op" empty toto in
  let%bind () = expect_eq program "slice_op" tata at in
  let%bind () = expect_fail program "slice_op" foo in
  let%bind () = expect_fail program "slice_op" ba in
  let%bind b1 = Test_helpers.run_typed_program_with_imperative_input program"hasherman" foo in
  let%bind () = expect_eq_core program "hasherman" foo b1 in
  let%bind b3 = Test_helpers.run_typed_program_with_imperative_input program "hasherman" foototo in
  let%bind () = trace_assert_fail_option (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b3 , b1) in
  ok ()

let set_arithmetic () : (unit, _) result =
  let%bind program = type_file "./contracts/set_arithmetic.ligo" in
  let%bind program_1 = type_file "./contracts/set_arithmetic-1.ligo" in
  let%bind () =
    expect_eq program_1 "iter_op"
      (e_set [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 13) in
  let%bind () =
    expect_eq program "add_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let%bind () =
    expect_eq program "add_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let%bind () =
    expect_eq program "remove_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program "remove_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program "remove_syntax"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program "remove_deep"
      (e_pair
         (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
         (e_nat 42))
      (e_pair
        (e_set [e_string "foo" ; e_string "bar"])
        (e_nat 42))
  in
  let%bind () =
    expect_eq program "patch_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"; e_string "foobar"]) in
  let%bind () =
    expect_eq program "patch_op_deep"
      (e_pair
         (e_set [e_string "foo" ; e_string "bar"])
         (e_nat 42))
      (e_pair
         (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
         (e_nat 42)) in
  let%bind () =
    expect_eq program "mem_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_bool true) in
  let%bind () =
    expect_eq program "mem_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_bool false) in
  let%bind () =
    expect_eq program_1 "fold_op"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_int 29)
  in
  ok ()

let set_arithmetic_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/set_arithmetic.mligo" in
  let%bind program_1 = type_file "./contracts/set_arithmetic-1.ligo" in
  let%bind () =
    expect_eq program "literal_op"
      (e_unit ())
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
  in
  let%bind () =
    expect_eq program "size_op"
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
      (e_nat 3) in
  let%bind () =
    expect_eq program "add_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let%bind () =
    expect_eq program "add_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let%bind () =
    expect_eq program "remove_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program "remove_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program_1 "fold_op"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_int 29)
  in
  ok ()

let set_arithmetic_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/set_arithmetic.religo" in
  let%bind program_1 = type_file "./contracts/set_arithmetic-1.ligo" in
  let%bind () =
    expect_eq program "literal_op"
      (e_unit ())
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
  in
  let%bind () =
    expect_eq program "size_op"
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
      (e_nat 3) in
  let%bind () =
    expect_eq program "add_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let%bind () =
    expect_eq program "add_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let%bind () =
    expect_eq program "remove_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program "remove_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program_1 "fold_op"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_int 29)
  in
  ok ()

let unit_expression () : (unit, _) result =
  let%bind program = type_file "./contracts/unit.ligo" in
  expect_eq_evaluate program "u" (e_unit ())

let string_expression () : (unit, _) result =
  let%bind program = type_file "./contracts/string.ligo" in
  let%bind _ = expect_eq_evaluate program "s" (e_string "toto") in
  expect_eq_evaluate program "y" (e_string "foototobar")

let include_ () : (unit, _) result =
  let%bind program = type_file "./contracts/includer.ligo" in
  expect_eq_evaluate program "bar" (e_int 144)

let include_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/includer.mligo" in
  expect_eq_evaluate program "bar" (e_int 144)

let include_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/includer.religo" in
  expect_eq_evaluate program "bar" (e_int 144)

let record_ez_int names n =
  e_record_ez @@ List.map (fun x -> x, e_int n) names

let tuple_ez_int names n =
  e_tuple @@ List.map (fun _ -> e_int n) names

let multiple_parameters () : (unit, _) result  =
  let%bind program = type_file "./contracts/multiple-parameters.ligo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n program name make_input make_output'
  in
  let%bind _ = bind_list @@ List.map aux [
      ("ab", tuple_ez_int ["a";"b"], fun n -> 2 * n) ;
      ("abcd", tuple_ez_int ["a";"b";"c";"d"], fun n -> 4 * n + 2) ;
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ok ()

let multiple_parameters_mligo () : (unit, _) result  =
  let%bind program = mtype_file "./contracts/multiple-parameters.mligo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n program name make_input make_output'
  in
  let%bind _ = bind_list @@ List.map aux [
      (* Didn't include the other tests because they're probably not necessary *)
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ok ()

let multiple_parameters_religo () : (unit, _) result  =
  let%bind program = retype_file "./contracts/multiple-parameters.religo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n program name make_input make_output'
  in
  let%bind _ = bind_list @@ List.map aux [
      (* Didn't include the other tests because they're probably not necessary *)
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ok ()

let record () : (unit, _) result  =
  let%bind program = type_file "./contracts/record.ligo" in
  let%bind () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let%bind () = expect_eq_evaluate program "a" (e_int 42) in
    let%bind () = expect_eq_evaluate program "b" (e_int 142) in
    let%bind () = expect_eq_evaluate program "c" (e_int 242) in
    ok ()
  in
  let%bind () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection" make_input make_expected
  in
  let%bind () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_record_ez [("foo" , e_int 256) ; ("bar" , e_int n) ] in
    expect_eq_n program "modify" make_input make_expected
  in
  let%bind () =
    let make_input = record_ez_int ["a" ; "b" ; "c"] in
    let make_expected = fun n -> e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int 42)
      ] in
    expect_eq_n program "modify_abc" make_input make_expected
  in
  let%bind () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_eq_evaluate program "br" expected
  in
  let%bind () =
    let make_input = fun n -> e_record_ez [("inner", record_ez_int ["a";"b";"c"] n)] in
    let make_expected = fun n -> e_record_ez [("inner", e_record_ez[
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int n)
    ])] in
    expect_eq_n program "modify_inner" make_input make_expected
  in
  ok ()

let record_mligo () : (unit, _) result  =
  let%bind program = mtype_file "./contracts/record.mligo" in
  let%bind () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let%bind () = expect_eq_evaluate program "a" (e_int 42) in
    let%bind () = expect_eq_evaluate program "b" (e_int 142) in
    let%bind () = expect_eq_evaluate program "c" (e_int 242) in
    ok ()
  in
  let%bind () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection" make_input make_expected
  in
  let%bind () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_record_ez [("foo" , e_int 256) ; ("bar" , e_int n) ] in
    expect_eq_n program "modify" make_input make_expected
  in
  let%bind () =
    let make_input = record_ez_int ["a" ; "b" ; "c"] in
    let make_expected = fun n -> e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int 42)
      ] in
    expect_eq_n program "modify_abc" make_input make_expected
  in
  let%bind () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_eq_evaluate program "br" expected
  in
  let%bind () =
    let make_input = fun n -> e_record_ez [("inner", record_ez_int ["a";"b";"c"] n)] in
    let make_expected = fun n -> e_record_ez [("inner", e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int n)
    ])] in
    expect_eq_n program "modify_inner" make_input make_expected
  in
  ok ()

let record_religo () : (unit, _) result  =
  let%bind program = retype_file "./contracts/record.religo" in
  let%bind () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let%bind () = expect_eq_evaluate program "a" (e_int 42) in
    let%bind () = expect_eq_evaluate program "b" (e_int 142) in
    let%bind () = expect_eq_evaluate program "c" (e_int 242) in
    ok ()
  in
  let%bind () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection" make_input make_expected
  in
  let%bind () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_record_ez [("foo" , e_int 256) ; ("bar" , e_int n) ] in
    expect_eq_n program "modify" make_input make_expected
  in
  let%bind () =
    let make_input = record_ez_int ["a" ; "b" ; "c"] in
    let make_expected = fun n -> e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int 42)
      ] in
    expect_eq_n program "modify_abc" make_input make_expected
  in
  let%bind () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_eq_evaluate program "br" expected
  in
  let%bind () =
    let make_input = fun n -> e_record_ez [("inner", record_ez_int ["a";"b";"c"] n)] in
    let make_expected = fun n -> e_record_ez [("inner", e_record_ez[
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int n)
    ])] in
    expect_eq_n program "modify_inner" make_input make_expected
  in
  ok ()

let tuple () : (unit, _) result  =
  let%bind program = type_file "./contracts/tuple.ligo" in
  let ez n =
    e_tuple (List.map e_int n) in
  let%bind () =
    let expected = ez [0 ; 0] in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; 2 * n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection_abc" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; n ; n] in
    let make_expected = fun n -> ez [n ; 2048 ; n] in
    expect_eq program "modify_abc" (make_input 12) (make_expected 12)
  in
  let%bind () =
    let make_input = fun n -> ez [n ; n ; n] in
    let make_expected = fun n -> ez [n ; 2048 ; n] in
    expect_eq_n program "modify_abc" make_input make_expected
  in
  let%bind () =
    let expected = ez [0 ; 1 ; 2 ; 3 ; 4; 5; 6; 7; 8; 9; 10; 11] in
    expect_eq_evaluate program "br" expected
  in
  let%bind () =
    let make_input = fun n -> ez [n; n; n; n; n; n; n; n; n; n; n; n] in
    let make_expected = fun n -> ez [n; n; n; n; n; n; n; n; n; n; n; 2048] in
    expect_eq_n program "update" make_input make_expected
  in
  ok ()

let tuple_mligo () : (unit, _) result  =
  let%bind program = mtype_file "./contracts/tuple.mligo" in
  let ez n =
    e_tuple (List.map e_int n) in
  let%bind () =
    let expected = ez [0 ; 0] in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; 2 * n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection_abc" make_input make_expected
  in
  let%bind () =
    let expected = ez [23 ; 23 ; 23 ; 23 ; 23] in
    expect_eq_evaluate program "br" expected
  in
  ok ()


let tuple_religo () : (unit, _) result  =
  let%bind program = retype_file "./contracts/tuple.religo" in
  let ez n =
    e_tuple (List.map e_int n) in
  let%bind () =
    let expected = ez [0 ; 0] in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; 2 * n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection_abc" make_input make_expected
  in
  let%bind () =
    let expected = ez [23 ; 23 ; 23 ; 23 ; 23] in
    expect_eq_evaluate program "br" expected
  in
  ok ()

let option () : (unit, _) result =
  let%bind program = type_file "./contracts/option.ligo" in
  let%bind () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate program "s" expected
  in
  let%bind () =
    let expected = e_typed_none (t_int ()) in
    expect_eq_evaluate program "n" expected
  in
  let%bind () =
    let expected = e_typed_none (t_int ()) in
    expect_eq program "assign" (e_int 12) expected
  in
  ok ()

let moption () : (unit, _) result =
  let%bind program = mtype_file "./contracts/option.mligo" in
  let%bind () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate program "s" expected
  in
  let%bind () =
    let expected = e_typed_none (t_int ()) in
    expect_eq_evaluate program "n" expected
  in
  ok ()

let reoption () : (unit, _) result =
  let%bind program = retype_file "./contracts/option.religo" in
  let%bind () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate program "s" expected
  in
  let%bind () =
    let expected = e_typed_none (t_int ()) in
    expect_eq_evaluate program "n" expected
  in
  ok ()


let map_ type_f path : (unit, _) result =
  let%bind program = type_f path in
  let ez lst =
    let lst' = List.map (fun (x, y) -> e_int x, e_int y) lst in
    e_typed_map lst' (t_int ()) (t_int ())
  in
   let%bind () =
    let make_input = fun n ->
      let m = ez [(23 , 0) ; (42 , 0)] in
      e_tuple [(e_int n) ; m]
    in
    let make_expected = fun n -> ez [(23 , n) ; (42 , 0)] in
    expect_eq_n_pos_small program "set_" make_input make_expected
  in
  let%bind () =
    let input = (e_pair (e_int 23) (ez [(42, 42)])) in
    let expected = ez [(23, 23) ; (42, 42)] in
    expect_eq program "add" input expected
  in
  let%bind () =
    let input = ez [(23, 23) ; (42, 42)] in
    let expected = ez [23, 23] in
    expect_eq program "rm" input expected
  in
  let%bind () =
    let input = ez [(0,0) ; (1,1) ; (2,2)] in
    let expected = ez [(0, 5) ; (1, 6) ; (2, 7)] in
    expect_eq program "patch_" input expected
  in
  let%bind () =
    let input = (e_pair
                   (ez [(0,0) ; (1,1) ; (2,2)])
                   (e_nat 10)) in
    let expected = (e_pair
                      (ez [(0,0) ; (1,9) ; (2,2)])
                      (e_nat 10)) in
    expect_eq program "patch_deep" input expected
  in
  let%bind () =
    let make_input = fun n -> ez List.(map (fun x -> (x, x)) @@ range n) in
    let make_expected = e_nat in
    expect_eq_n_strict_pos_small program "size_" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_some @@ e_int 4 in
    expect_eq_n program "get" make_input make_expected
  in
  let%bind () =
    let input_map = ez [(23, 10) ; (42, 4)] in
    expect_eq program "mem" (e_tuple [(e_int 23) ; input_map]) (e_bool true)
  in
  let%bind () =
    let input_map = ez [(23, 10) ; (42, 4)] in
    expect_eq program "mem" (e_tuple [(e_int 1000) ; input_map]) (e_bool false)
  in
  let%bind () = expect_eq_evaluate program "empty_map"
    (e_annotation (e_map []) (t_map (t_int()) (t_int()))) in
  let%bind () =
    let expected = ez @@ List.map (fun x -> (x, 23)) [144 ; 51 ; 42 ; 120 ; 421] in
    expect_eq_evaluate program "map1" expected
  in
  let%bind () =
    let expected = ez [(23, 0) ; (42, 0)] in
    expect_eq_evaluate program "map2" expected
  in
  let%bind () =
    let input = ez [(1 , 1) ; (2 , 2) ; (3 , 3) ] in
    let expected = e_unit () in
    expect_eq program "iter_op" input expected
  in
  let%bind () =
    let input = ez [(1 , 10) ; (2 , 20) ; (3 , 30) ] in
    let expected = ez [(1 , 11) ; (2 , 21) ; (3 , 31) ] in
    expect_eq program "map_op" input expected
  in
  let%bind () =
    let input = ez [(1 , 10) ; (2 , 20) ; (3 , 30) ] in
    let expected = e_int 76 in
    expect_eq program "fold_op" input expected
  in
  let%bind () =
    let input = ez [(2 , 20) ; (42 , 10)] in
    let expected = ez [(2 , 20) ; (32 , 16) ] in
    expect_eq program "deep_op" input expected
  in
  ok ()

let big_map_ type_f path : (unit, _) result =
  let%bind program = type_f path in
  let ez lst =
    let lst' = List.map (fun (x, y) -> e_int x, e_int y) lst in
    (e_typed_big_map lst' (t_int ()) (t_int()))
  in
  let%bind () =
    let make_input = fun n ->
      let m = ez [(23 , 0) ; (42 , 0)] in
      e_tuple [(e_int n) ; m]
    in
    let make_expected = fun n -> ez [(23 , n) ; (42 , 0)] in
    expect_eq_n_pos_small program "set_" make_input make_expected
  in
  let%bind () =
    let input = (e_pair (e_int 23) (ez [(42, 42)])) in
    let expected = ez [(23, 23) ; (42, 42)] in
    expect_eq program "add" input expected
  in
  let%bind () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_some @@ e_int 4 in
    expect_eq_n program "get" make_input make_expected
  in
  let%bind () =
    let input = ez [(23, 23) ; (42, 42)] in
    let expected = ez [23, 23] in
    expect_eq program "rm" input expected
  in
  ok ()


let map () : (unit, _) result = map_ type_file "./contracts/map.ligo"
let mmap () : (unit, _) result = map_ mtype_file "./contracts/map.mligo"
let remap () : (unit, _) result = map_ retype_file "./contracts/map.religo"
let big_map () : (unit, _) result = big_map_ type_file "./contracts/big_map.ligo"
let mbig_map () : (unit, _) result = big_map_ mtype_file "./contracts/big_map.mligo"
let rebig_map () : (unit, _) result = big_map_ retype_file "./contracts/big_map.religo"


let list () : (unit, _) result =
  Format.printf "Pre_type \n%!";
  let%bind program = type_file "./contracts/list.ligo" in
  let ez lst =
    let lst' = List.map e_int lst in
    e_typed_list lst' (t_int ())
  in
  Format.printf "Post_type \n%!";
  let%bind () =
    let expected = ez [23 ; 42] in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let expected = ez [144 ; 23 ; 42] in
    expect_eq_evaluate program "fb2" expected
  in
  let%bind () =
    let expected = ez [688 ; 144 ; 23 ; 42] in
    expect_eq_evaluate program "fb3" expected
  in
  let%bind () =
    let expected = e_some @@ e_int 23 in
    expect_eq_evaluate program "fb_head" expected
  in
  let%bind () =
    let expected = e_some @@ ez [42] in
    expect_eq_evaluate program "fb_tail" expected
  in
  let%bind () =
    let make_input = fun n -> (ez @@ List.range n) in
    let make_expected = e_nat in
    expect_eq_n_strict_pos_small program "size_" make_input make_expected
  in
  let%bind () =
    let expected = ez [144 ; 51 ; 42 ; 120 ; 421] in
    expect_eq_evaluate program "bl" expected
  in
  let%bind () =
    expect_eq program "fold_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 23)
  in
  (* not working since purification (problem with effect in out of iter
  let%bind () =
    expect_eq program "iter_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 13)
  in
  *)
  let%bind () =
    expect_eq program "map_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_list [e_int 3 ; e_int 5 ; e_int 8])
  in
  ok ()

let condition () : (unit, _) result =
  let%bind program = type_file "./contracts/condition.ligo" in
  let%bind _ =
    let make_input = e_int in
    let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
    expect_eq_n program "main" make_input make_expected
  in
  let%bind _ =
    let make_expected = fun b -> e_int (if b then 42 else 1) in
    expect_eq_b program "foo" make_expected
  in
  ok ()

let condition_mligo () : (unit, _) result =
  let%bind _ =
    let aux file =
      let%bind program = mtype_file file in
      let make_input = e_int in
      let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
      expect_eq_n program "main"  make_input make_expected in
    bind_map_list aux [
      "./contracts/condition.mligo";
      "./contracts/condition-shadowing.mligo";
      "./contracts/condition-annot.mligo";
    ] in
  ok ()

let condition_religo () : (unit, _) result =
  let%bind _ =
    let aux file =
      let%bind program = retype_file file in
      let make_input = e_int in
      let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
      expect_eq_n program "main"  make_input make_expected in
    bind_map_list aux [
      "./contracts/condition.religo";
      "./contracts/condition-shadowing.religo";
      "./contracts/condition-annot.religo";
    ] in
  ok ()

let sequence_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/sequence.mligo" in
  expect_eq program "y" (e_unit ()) (e_nat 1)

let eq_bool_common program =
  let%bind _ =
    bind_map_list (fun ( a , b , expected ) ->
        expect_eq program "main" (e_pair (e_bool a) (e_bool b)) (e_int expected))
    [
      ( false , false , 999 ) ;
      ( false , true  , 1   ) ;
      ( true  , false , 1   ) ;
      ( true  , true  , 999 ) ;
    ]
  in
  ok ()

let eq_bool () : (unit, _) result =
  let%bind program = type_file "./contracts/eq_bool.ligo" in
  eq_bool_common program

let eq_bool_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/eq_bool.mligo" in
  eq_bool_common program

let eq_bool_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/eq_bool.religo" in
  eq_bool_common program

let condition_simple () : (unit, _) result =
  let%bind program = type_file "./contracts/condition-simple.ligo" in
  let make_input = e_int in
  let make_expected = fun _ -> e_int 42 in
  expect_eq_n program "main" make_input make_expected

let loop () : (unit, _) result =
  let%bind program = type_file "./contracts/loop.ligo" in
  let%bind () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos program "dummy" make_input make_expected in
  let%bind () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos_mid program "counter" make_input make_expected in
  let%bind () =
    let make_input = e_nat in
    let make_expected = fun n -> e_nat (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "while_sum" make_input make_expected in
  let%bind () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "for_sum" make_input make_expected in
  let%bind () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * n) in
    expect_eq_n_pos_mid program "for_sum_step" make_input make_expected in
  let input = e_unit () in
  let%bind () =
    let expected = e_pair (e_int 3) (e_string "totototo") in
    expect_eq program "for_collection_list" input expected in
  let%bind () =
    let expected = e_pair (e_int 6) (e_string "totototo") in
    expect_eq program "for_collection_set" input expected in
  let%bind () =
    let expected = e_pair (e_int 6) (e_string "123") in
    expect_eq program "for_collection_map_kv" input expected in
  let%bind () =
    let expected = (e_int 0) in
    expect_eq program "for_collection_empty" input expected in
  let%bind () =
    let expected = (e_int 13) in
    expect_eq program "for_collection_if_and_local_var" input expected in
  let%bind () =
    let expected = (e_int 1020) in
    expect_eq program "for_collection_rhs_capture" input expected in
  let%bind () =
    let expected = (e_int 1040) in
    expect_eq program "for_collection_proc_call" input expected in
  let%bind () =
    let expected = (e_int 20) in
    expect_eq program "for_collection_comp_with_acc" input expected in
  let%bind () =
    let expected = e_pair (e_int 24)
      (e_string "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two ") in
    expect_eq program "nested_for_collection" input expected in
  let%bind () =
    let expected = e_pair (e_int 24)
      (e_string "123123123") in
    expect_eq program "nested_for_collection_local_var" input expected in
  let%bind () =
    let expected = e_pair (e_bool true) (e_int 4) in
    expect_eq program "inner_capture_in_conditional_block"  input expected in
  let%bind () =
    let ez lst =
      let lst' = List.map (fun (x, y) -> e_string x, e_int y) lst in
        e_typed_map lst' (t_string ()) (t_int ())
    in
    let expected = ez [ ("I" , 12) ; ("am" , 12) ; ("foo" , 12) ] in
    expect_eq program "for_collection_with_patches" input expected in
  ok ()

(* Don't know how to assert parse error happens in this test framework
let for_fail () : (unit, _) result =
  let%bind program = type_file "./contracts/for_fail.ligo" in
  let%bind () = expect_fail program "main" (e_nat 0)
  in ok () *)

let loop_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/loop.mligo" in
  let%bind () =
    let input = e_int 0 in
    let expected = e_int 100 in
    expect_eq program "counter_simple" input expected
  in
  let%bind () =
    let input = e_int 100 in
    let expected = e_int 5050 in
    expect_eq program "counter" input expected
  in
  let%bind () =
    let input = e_int 100 in
    let expected = e_int 10000 in
    expect_eq program "counter_nest" input expected
  in ok ()

let loop_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/loop.religo" in
  let%bind () =
    let input = e_int 0 in
    let expected = e_int 100 in
    expect_eq program "counter_simple" input expected
  in
  let%bind () =
    let input = e_int 100 in
    let expected = e_int 5050 in
    expect_eq program "counter" input expected
  in
  let%bind () =
    let input = e_int 100 in
    let expected = e_int 10000 in
    expect_eq program "counter_nest" input expected
  in ok ()


let matching () : (unit, _) result =
  let%bind program = type_file "./contracts/match.ligo" in
  let%bind () =
    let make_input = e_int in
    let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
    expect_eq_n program "match_bool" make_input make_expected
  in
  let%bind () =
    let make_input = e_int in
    let make_expected = fun n-> e_int (if n = 2 then 42 else 0) in
    expect_eq_n program "match_expr_bool" make_input make_expected
  in
  let%bind () =
    let aux n =
      let input = match n with
        | Some s -> e_some (e_int s)
        | None -> e_typed_none (t_int ()) in
      let expected = e_int (match n with
          | Some s -> s
          | None -> 23) in
      expect_eq program "match_option" input expected
    in
    bind_iter_list aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  let%bind () =
    let aux n =
      let input = match n with
        | Some s -> e_some (e_int s)
        | None -> e_typed_none (t_int ()) in
      let expected = e_int (match n with
          | Some s -> s
          | None -> 42) in
      expect_eq program "match_expr_option" input expected
    in
    bind_iter_list aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  let%bind () =
    let aux lst = e_annotation (e_list @@ List.map e_int lst) (t_list (t_int ())) in
    let%bind () = expect_eq program "match_expr_list" (aux [ 14 ; 2 ; 3 ]) (e_int 14) in
    let%bind () = expect_eq program "match_expr_list" (aux [ 13 ; 2 ; 3 ]) (e_int 13) in
    let%bind () = expect_eq program "match_expr_list" (aux []) (e_int (-1)) in
    ok ()
  in
  ok ()

let declarations () : (unit, _) result =
  let%bind program = type_file "./contracts/declarations.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (42 + n) in
  expect_eq program "main" (make_input 0) (make_expected 0) >>? fun () ->
  expect_eq_n program "main" make_input make_expected

let declaration_local () : (unit, _) result =
  let%bind program = type_file "./contracts/declaration-local.ligo" in
  let make_input = e_int in
  let make_expected = fun _ -> e_int 42 in
  expect_eq_n program "main" make_input make_expected

let quote_declaration () : (unit, _) result =
  let%bind program = type_file "./contracts/quote-declaration.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (42 + 2 * n) in
  expect_eq_n program "main" make_input make_expected

let quote_declarations () : (unit, _) result =
  let%bind program = type_file "./contracts/quote-declarations.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (74 + 2 * n) in
  expect_eq_n program "main" make_input make_expected

let counter_contract () : (unit, _) result =
  let%bind program = type_file "./contracts/counter.ligo" in
  let make_input = fun n-> e_pair (e_int n) (e_int 42) in
  let make_expected = fun n -> e_pair (e_typed_list [] (t_operation ())) (e_int (42 + n)) in
  expect_eq_n program "main" make_input make_expected

let super_counter_contract () : (unit, _) result =
  let%bind program = type_file "./contracts/super-counter.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected

let super_counter_contract_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/super-counter.mligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected

let super_counter_contract_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/super-counter.religo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation())) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected


let dispatch_counter_contract () : (unit, _) result =
  let%bind program = type_file "./contracts/dispatch-counter.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation())) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected

let failwith_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/failwith.ligo" in
  let should_fail = expect_fail program "main" in
  let should_work input = expect_eq program "main" input (e_pair (e_typed_list [] (t_operation())) (e_unit ())) in
  let%bind _ = should_work (e_pair (e_constructor "Zero" (e_nat 0)) (e_unit ())) in
  let%bind _ = should_fail (e_pair (e_constructor "Zero" (e_nat 1)) (e_unit ())) in
  let%bind _ = should_work (e_pair (e_constructor "Pos" (e_nat 1)) (e_unit ())) in
  let%bind _ = should_fail (e_pair (e_constructor "Pos" (e_nat 0)) (e_unit ())) in
  let should_fail input = expect_fail program "foobar" (e_int input) in
  let should_work input n = expect_eq program "foobar" (e_int input) (e_int n) in
  let%bind () = should_fail 10 in
  let%bind () = should_fail @@ -10 in
  let%bind () = should_work 5 6 in
  ok ()

let failwith_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/failwith.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  expect_fail program "main" make_input

let failwith_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/failwith.religo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  expect_fail program "main" make_input

let assert_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/assert.mligo" in
  let make_input b = e_pair (e_bool b) (e_unit ()) in
  let make_expected = e_pair (e_typed_list [] (t_operation())) (e_unit ()) in
  let%bind _ = expect_fail program "main" (make_input false) in
  let%bind _ = expect_eq program "main" (make_input true) make_expected in
  let%bind _ = expect_fail program "some" (e_none ()) in
  let%bind _ = expect_eq program "some" (e_some (e_unit ())) (e_unit ()) in
  ok ()

let assert_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/assert.religo" in
  let make_input b = e_pair (e_bool b) (e_unit ()) in
  let make_expected = e_pair (e_typed_list [] (t_operation())) (e_unit ()) in
  let%bind _ = expect_fail program "main" (make_input false) in
  let%bind _ = expect_eq program "main" (make_input true) make_expected in
  ok ()

let recursion_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/recursion.ligo" in
  let%bind _ =
    let make_input = e_pair (e_int 10) (e_int 0) in
    let make_expected = e_int 55 in
    expect_eq program "sum" make_input make_expected
  in
  let%bind _ =
    let make_input = e_tuple [(e_int 10); (e_int 1); (e_int 1)] in
    let make_expected = e_int 89 in
    expect_eq program "fibo" make_input make_expected
  in ok ()


let recursion_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/recursion.mligo" in
  let%bind _ =
    let make_input = e_pair (e_int 10) (e_int 0) in
    let make_expected = e_int 55 in
    expect_eq program "sum" make_input make_expected
  in
  let%bind _ =
    let make_input = e_tuple [(e_int 10); (e_int 1); (e_int 1)] in
    let make_expected = e_int 89 in
    expect_eq program "fibo" make_input make_expected
  in ok ()

let recursion_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/recursion.religo" in
  let%bind _ =
    let make_input = e_pair (e_int 10) (e_int 0) in
    let make_expected = e_int 55 in
    expect_eq program "sum" make_input make_expected
  in
  let%bind _ =
    let make_input = e_tuple [(e_int 10); (e_int 1); (e_int 1)] in
    let make_expected = e_int 89 in
    expect_eq program "fibo" make_input make_expected
  in ok ()

let guess_string_mligo () : (unit, _) result =
  let%bind program = type_file "./contracts/guess_string.mligo" in
  let make_input = fun n -> e_pair (e_int n) (e_int 42) in
  let make_expected = fun n -> e_pair (e_typed_list [] (t_operation())) (e_int (42 + n))
  in expect_eq_n program "main" make_input make_expected

let basic_mligo () : (unit, _) result =
  let%bind typed = mtype_file "./contracts/basic.mligo" in
  expect_eq_evaluate typed "foo" (e_int (42+127))

let basic_religo () : (unit, _) result =
  let%bind typed = retype_file "./contracts/basic.religo" in
  expect_eq_evaluate typed "foo" (e_int (42+127))

let counter_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/counter.mligo" in
  let make_input n = e_pair (e_int n) (e_int 42) in
  let make_expected n = e_pair (e_typed_list [] (t_operation ())) (e_int (42 + n)) in
  expect_eq_n program "main" make_input make_expected

let counter_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/counter.religo" in
  let make_input n = e_pair (e_int n) (e_int 42) in
  let make_expected n = e_pair (e_typed_list [] (t_operation ())) (e_int (42 + n)) in
  expect_eq_n program "main" make_input make_expected


let let_in_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/letin.mligo" in
  let%bind () =
    let make_input n = e_pair (e_int n) (e_pair (e_int 3) (e_int 5)) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_pair (e_int (7+n)) (e_int (3+5)))
    in
    expect_eq_n program "main" make_input make_expected
  in
  let%bind () =
    expect_eq program "letin_nesting" (e_unit ()) (e_string "test")
  in
  let%bind () =
    expect_eq program "letin_nesting2" (e_int 4) (e_int 9)
  in
  ok ()

let let_in_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/letin.religo" in
  let%bind () =
    let make_input n = e_pair (e_int n) (e_pair (e_int 3) (e_int 5)) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_pair (e_int (7+n)) (e_int (3+5)))
    in
    expect_eq_n program "main" make_input make_expected
  in
  let%bind () =
    expect_eq program "letin_nesting" (e_unit ()) (e_string "test")
  in
  let%bind () =
    expect_eq program "letin_nesting2" (e_int 4) (e_int 9)
  in
  ok ()

let local_type_decl program : (unit, _) result =
  let%bind () =
    expect_eq program "local_type" (e_unit ()) (e_int 3)
  in
  ok ()

let local_type_decl_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/local_type_decl.ligo" in
  local_type_decl program

let local_type_decl_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/local_type_decl.mligo" in
  local_type_decl program

let local_type_decl_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/local_type_decl.religo" in
  local_type_decl program

let match_variant () : (unit, _) result =
  let%bind program = mtype_file "./contracts/match.mligo" in
  let%bind () =
    let make_input n =
      e_pair (e_constructor "Sub" (e_int n)) (e_int 3) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n program "main" make_input make_expected in
  let%bind () =
    let input = e_bool true in
    let expected = e_int 10 in
    expect_eq program "match_bool" input expected in
  let%bind () =
    let input = e_bool false in
    let expected = e_int 0 in
    expect_eq program "match_bool" input expected in
  let%bind () =
    let input = e_list [e_int 3] in
    let expected = e_int 3 in
    expect_eq program "match_list" input expected in
  let%bind () =
    let input = e_typed_list [] (t_int ()) in
    let expected = e_int 10 in
    expect_eq program "match_list" input expected in
  let%bind () =
    let make_input n = e_some (e_int n) in
    let make_expected n = e_int n in
    expect_eq_n program "match_option" make_input make_expected in
  ok ()

let match_variant_re () : (unit, _) result =
  let%bind program = retype_file "./contracts/match.religo" in
  let make_input n =
    e_pair (e_constructor "Sub" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n program "main" make_input make_expected


let match_matej () : (unit, _) result =
  let%bind program = mtype_file "./contracts/match_bis.mligo" in
  let make_input n =
    e_pair (e_constructor "Decrement" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n program "main" make_input make_expected

let match_matej_re () : (unit, _) result =
  let%bind program = retype_file "./contracts/match_bis.religo" in
  let make_input n =
    e_pair (e_constructor "Decrement" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n program "main" make_input make_expected


let mligo_list () : (unit, _) result =
  let%bind program = mtype_file "./contracts/list.mligo" in
  let%bind () = expect_eq program "size_" (e_list [e_int 0; e_int 1; e_int 2]) (e_nat 3) in
  let aux lst = e_list @@ List.map e_int lst in
  let%bind () = expect_eq program "fold_op" (aux [ 1 ; 2 ; 3 ]) (e_int 16) in
  let%bind () =
    let make_input n =
      e_pair (e_list [e_int n; e_int (2*n)])
        (e_pair (e_int 3) (e_list [e_int 8])) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ()))
        (e_pair (e_int (n+3)) (e_list [e_int (2*n)]))
    in
    expect_eq_n program "main" make_input make_expected
  in
  let%bind () = expect_eq_evaluate program "x" (e_list []) in
  let%bind () = expect_eq_evaluate program "y" (e_list @@ List.map e_int [3 ; 4 ; 5]) in
  let%bind () = expect_eq_evaluate program "z" (e_list @@ List.map e_int [2 ; 3 ; 4 ; 5]) in
  let%bind () = expect_eq program "map_op" (aux [2 ; 3 ; 4 ; 5]) (aux [3 ; 4 ; 5 ; 6]) in
  let%bind () = expect_eq program "iter_op" (aux [2 ; 3 ; 4 ; 5]) (e_unit ()) in
  ok ()

let religo_list () : (unit, _) result =
  let%bind program = retype_file "./contracts/list.religo" in
  let%bind () = expect_eq program "size_" (e_list [e_int 0; e_int 1; e_int 2]) (e_nat 3) in
  let aux lst = e_list @@ List.map e_int lst in
  let%bind () = expect_eq program "fold_op" (aux [ 1 ; 2 ; 3 ]) (e_int 16) in
  let%bind () =
    let make_input n =
      e_pair (e_list [e_int n; e_int (2*n)])
        (e_pair (e_int 3) (e_list [e_int 8])) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ()))
        (e_pair (e_int (n+3)) (e_list [e_int (2*n)]))
    in
    expect_eq_n program "main" make_input make_expected
  in
  let%bind () = expect_eq_evaluate program "x" (e_list []) in
  let%bind () = expect_eq_evaluate program "y" (e_list @@ List.map e_int [3 ; 4 ; 5]) in
  let%bind () = expect_eq_evaluate program "z" (e_list @@ List.map e_int [2 ; 3 ; 4 ; 5]) in
  let%bind () = expect_eq program "map_op" (aux [2 ; 3 ; 4 ; 5]) (aux [3 ; 4 ; 5 ; 6]) in
  let%bind () = expect_eq program "iter_op" (aux [2 ; 3 ; 4 ; 5]) (e_unit ()) in
  ok ()

let lambda_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/lambda.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq program "main" make_input make_expected

let lambda_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/lambda.religo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq program "main" make_input make_expected


let lambda_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/lambda.ligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq program "main" make_input make_expected

let lambda2_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/lambda2.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq program "main" make_input make_expected

let lambda2_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/lambda2.religo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq program "main" make_input make_expected


let fibo_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/fibo.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_int 42) in
  expect_eq program "main" make_input make_expected

let michelson_insertion program : (unit, _) result =
  let%bind program = program in
  let make_input = fun n -> e_pair (e_nat n) (e_nat 1) in
  let make_expected = fun n -> e_nat (n+1) in
  expect_eq_n_pos program "michelson_add" make_input make_expected

let michelson_insertion_ligo () : (unit, _) result =
  michelson_insertion @@ type_file "./contracts/michelson_insertion.ligo"

let michelson_insertion_mligo () : (unit, _) result =
  michelson_insertion @@ mtype_file "./contracts/michelson_insertion.mligo"

let michelson_insertion_religo () : (unit, _) result =
  michelson_insertion @@ retype_file "./contracts/michelson_insertion.religo"

let website1_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/website1.ligo" in
  let make_input = fun n-> e_pair (e_int n) (e_int 42) in
  let make_expected = fun _n -> e_pair (e_typed_list [] (t_operation ())) (e_int (42 + 1)) in
  expect_eq_n program "main" make_input make_expected

let website2_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/website2.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected

let tez_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/tez.ligo" in
  let%bind _ = expect_eq_evaluate program "add_tez" (e_mutez 42) in
  let%bind _ = expect_eq_evaluate program "sub_tez" (e_mutez 1) in
  let%bind _ = expect_eq_evaluate program "not_enough_tez" (e_mutez 4611686018427387903) in
  let%bind _ = expect_eq_evaluate program "nat_mul_tez" (e_mutez 100) in
  let%bind _ = expect_eq_evaluate program "tez_mul_nat" (e_mutez 1000) in
  let%bind _ = expect_eq_evaluate program "tez_div_tez1" (e_nat 100) in
  let%bind _ = expect_eq_evaluate program "tez_div_tez2" (e_nat 1) in
  let%bind _ = expect_eq_evaluate program "tez_div_tez3" (e_nat 0) in
  let%bind _ = expect_eq_evaluate program "tez_mod_tez1" (e_mutez 0) in
  let%bind _ = expect_eq_evaluate program "tez_mod_tez2" (e_mutez 10) in
  let%bind _ = expect_eq_evaluate program "tez_mod_tez3" (e_mutez 100) in
  ok ()

let tez_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/tez.mligo" in
  let%bind _ = expect_eq_evaluate program "add_tez" (e_mutez 42) in
  let%bind _ = expect_eq_evaluate program "sub_tez" (e_mutez 1) in
  let%bind _ = expect_eq_evaluate program "not_enough_tez" (e_mutez 4611686018427387903) in
  let%bind _ = expect_eq_evaluate program "add_more_tez" (e_mutez 111111000) in
  ok ()

let website2_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/website2.mligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected

let website2_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/website2.religo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected


let mligo_let_multiple () : (unit, _) result =
  let%bind program = mtype_file "./contracts/let_multiple.mligo" in
  let%bind () =
    let input = e_unit () in
    let expected = e_int 3 in
    expect_eq program "main" input expected
  in
  let%bind () =
    let input = e_unit () in
    let expected = e_int 6 in
    expect_eq program "main_paren" input expected
  in
  let%bind () =
    let input = e_unit () in
    let expected = e_tuple [e_int 23 ; e_int 42] in
    expect_eq program "correct_values_bound" input expected
  in
  let%bind () =
    let input = e_unit () in
    let expected = e_int 19 in
    expect_eq program "non_tuple_rhs" input expected
  in
  let%bind () =
    let input = e_unit () in
    let expected = e_tuple [e_int 10; e_int 20; e_int 30; e_int 40; e_int 50] in
    expect_eq program "correct_values_big_tuple" input expected
  in
  let%bind () =
    let input = e_unit () in
    let expected = e_tuple [e_int 10 ; e_string "hello"] in
    expect_eq program "correct_values_different_types" input expected
  in
  ok ()

let religo_let_multiple () : (unit, _) result =
  let%bind program = retype_file "./contracts/let_multiple.religo" in
  let%bind () =
    let input = e_unit () in
    let expected = e_int 3 in
    expect_eq program "main" input expected
  in
  let%bind () =
    let input = e_unit () in
    let expected = e_int 6 in
    expect_eq program "main_paren" input expected
  in
  let%bind () =
    let input = e_unit () in
    let expected = e_int 65 in
    expect_eq program "non_tuple_rhs" input expected
  in
  ok ()


let balance_test_options () =
  let%bind balance = trace_option (test_internal "could not convert balance") @@
    Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "4000000" in
  ok @@ Proto_alpha_utils.Memory_proto_alpha.make_options ~balance ()

let balance_constant () : (unit, _) result =
  let%bind program = type_file "./contracts/balance_constant.ligo" in
  let input = e_tuple [e_unit () ; e_mutez 0]  in
  let expected = e_tuple [e_list []; e_mutez 4000000000000] in
  let%bind options = balance_test_options () in
  expect_eq ~options program "main" input expected


let balance_constant_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/balance_constant.mligo" in
  let input = e_tuple [e_unit () ; e_mutez 0]  in
  let expected = e_tuple [e_list []; e_mutez 4000000000000] in
  let%bind options = balance_test_options () in
  expect_eq ~options program "main" input expected

let balance_constant_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/balance_constant.religo" in
  let input = e_tuple [e_unit () ; e_mutez 0]  in
  let expected = e_tuple [e_list []; e_mutez 4000000000000] in
  let%bind options = balance_test_options () in
  expect_eq ~options program "main" input expected

let amount () : (unit, _) result =
  let%bind program = type_file "./contracts/amount.ligo" in
  let input = e_unit () in
  let expected = e_int 42 in
  let amount =
    match Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "100" with
    | Some t -> t
    | None -> Memory_proto_alpha.Protocol.Alpha_context.Tez.one
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount () in
  expect_eq ~options program "check" input expected

let amount_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/amount.mligo" in
  let input = e_unit () in
  let expected = e_int 42 in
  let amount =
    match Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "100" with
    | Some t -> t
    | None -> Memory_proto_alpha.Protocol.Alpha_context.Tez.one
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount () in
  expect_eq ~options program "check_" input expected

let amount_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/amount.religo" in
  let input = e_unit () in
  let expected = e_int 42 in
  let amount =
    match Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "100" with
    | Some t -> t
    | None -> Memory_proto_alpha.Protocol.Alpha_context.Tez.one
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount () in
  expect_eq ~options program "check_" input expected

let addr_test program =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth dummy_environment.identities 0).implicit_contract in
  let open Tezos_crypto in
  let key_hash = Signature.Public_key_hash.to_b58check @@
      (List.nth dummy_environment.identities 0).public_key_hash in
  expect_eq program "main" (e_key_hash key_hash) (e_address addr)

let address () : (unit, _) result =
  let%bind program = type_file "./contracts/address.ligo" in
  addr_test program

let address_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/address.mligo" in
  addr_test program

let address_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/address.religo" in
  addr_test program


let self_address () : (unit, _) result =
  let%bind _ = type_file "./contracts/self_address.ligo" in
  ok ()

let self_address_mligo () : (unit, _) result =
  let%bind _ = mtype_file "./contracts/self_address.mligo" in
  ok ()

let self_address_religo () : (unit, _) result =
  let%bind _ = retype_file "./contracts/self_address.religo" in
  ok ()

let implicit_account () : (unit, _) result =
  let%bind _ = type_file "./contracts/implicit_account.ligo" in
  ok ()

let implicit_account_mligo () : (unit, _) result =
  let%bind _ = mtype_file "./contracts/implicit_account.mligo" in
  ok ()


let implicit_account_religo () : (unit, _) result =
  let%bind _ = retype_file "./contracts/implicit_account.religo" in
  ok ()

let tuples_sequences_functions_religo () : (unit, _) result =
  let%bind _ = retype_file "./contracts/tuples_sequences_functions.religo" in
  ok ()

let is_nat () : (unit, _) result =
  let%bind program = type_file "./contracts/isnat.ligo" in
  let%bind () =
    let input = e_int 10 in
    let expected = e_some (e_nat 10) in
    expect_eq program "main" input expected
  in
  let%bind () =
    let input = e_int (-10) in
    let expected = e_none () in
    expect_eq program "main" input expected
  in ok ()

let is_nat_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/isnat.mligo" in
  let%bind () =
    let input = e_int 10 in
    let expected = e_some (e_nat 10) in
    expect_eq program "main" input expected
  in
  let%bind () =
    let input = e_int (-10) in
    let expected = e_none () in
    expect_eq program "main" input expected
  in ok ()

let is_nat_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/isnat.religo" in
  let%bind () =
    let input = e_int 10 in
    let expected = e_some (e_nat 10) in
    expect_eq program "main" input expected
  in
  let%bind () =
    let input = e_int (-10) in
    let expected = e_none () in
    expect_eq program "main" input expected
  in ok ()


let simple_access_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/simple_access.ligo" in
  let make_input = e_tuple [e_int 0; e_int 1] in
  let make_expected = e_int 2 in
  expect_eq program "main" make_input make_expected

let deep_access_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/deep_access.ligo" in
  let%bind () =
    let make_input = e_unit () in
    let make_expected = e_int 2 in
    expect_eq program "main" make_input make_expected in
  let%bind () =
    let make_input = e_unit () in
    let make_expected = e_int 6 in
    expect_eq program "asymetric_tuple_access" make_input make_expected in
  let%bind () =
    let make_input = e_record_ez [ ("nesty",
      e_record_ez [ ("mymap", e_typed_map [] (t_int ()) (t_string ())) ] ) ; ] in
    let make_expected = e_string "one" in
    expect_eq program "nested_record" make_input make_expected in
  ok ()

let attributes_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/attributes.ligo" in
  let%bind () =
    let input = e_int 3 in
    let expected = e_int 5 in
    expect_eq program "foo" input expected
  in
  ok ()

let attributes_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/attributes.mligo" in
  let%bind () =
    let input = e_int 3 in
    let expected = e_int 5 in
    expect_eq program "foo" input expected
  in
  ok ()

let attributes_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/attributes.religo" in
  let%bind () =
    let input = e_int 3 in
    let expected = e_int 5 in
    expect_eq program "foo" input expected
  in
  ok ()

let get_contract_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/get_contract.ligo" in
  let%bind () =
    let make_input = fun _n -> e_unit () in
    let make_expected : int -> Ast_core.expression -> (unit, _) result = fun _n result ->
      let%bind (ops , storage) = trace_option (test_internal __LOC__) @@ Ast_core.get_e_pair result.content in
      let%bind () =
        let%bind lst = trace_option (test_internal __LOC__) @@ Ast_core.get_e_list ops.content in
        Assert.assert_list_size (test_internal __LOC__) lst 1 in
      let expected_storage = Ast_core.e_unit () in
      trace_option (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (expected_storage , storage)
      in
    let%bind () =
      let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
      let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount () in
      let%bind () = expect_n_strict_pos_small ~options program "cb" make_input make_expected in
      expect_n_strict_pos_small ~options program "cbo" make_input make_expected in
    ok ()
  in
  ok()

let entrypoints_ligo () : (unit, _) result =
  let%bind _program = type_file "./contracts/entrypoints.ligo" in
  (* hmm... *)
  ok ()

let chain_id () : (unit, _) result =
  let%bind program = type_file "./contracts/chain_id.ligo" in
  let pouet = Tezos_crypto.Base58.simple_encode
    Tezos_base__TzPervasives.Chain_id.b58check_encoding
    Tezos_base__TzPervasives.Chain_id.zero in
  let make_input = e_chain_id pouet in
  let make_expected = e_chain_id pouet in
  let%bind () = expect_eq program "chain_id" make_input make_expected in
  ok ()

let key_hash () : (unit, _) result =
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let%bind program = type_file "./contracts/key_hash.ligo" in
  let make_input = e_pair (e_key_hash pkh_str) (e_key pk_str) in
  let make_expected = e_pair (e_bool true) (e_key_hash pkh_str) in
  let%bind () = expect_eq program "check_hash_key" make_input make_expected in
  ok ()

let key_hash_mligo () : (unit, _) result =
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let%bind program = mtype_file "./contracts/key_hash.mligo" in
  let make_input = e_pair (e_key_hash pkh_str) (e_key pk_str) in
  let make_expected = e_pair (e_bool true) (e_key_hash pkh_str) in
  let%bind () = expect_eq program "check_hash_key" make_input make_expected in
  ok ()

let key_hash_religo () : (unit, _) result =
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let%bind program = retype_file "./contracts/key_hash.religo" in
  let make_input = e_pair (e_key_hash pkh_str) (e_key pk_str) in
  let make_expected = e_pair (e_bool true) (e_key_hash pkh_str) in
  let%bind () = expect_eq program "check_hash_key" make_input make_expected in
  ok ()

let check_signature () : (unit, _) result =
  let open Tezos_crypto in
  let (_, raw_pk, sk) = Signature.generate_key () in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let signed = Signature.sign sk (Bytes.of_string "hello world") in
  let%bind program = type_file "./contracts/check_signature.ligo" in
  let make_input = e_tuple [e_key pk_str ;
                            e_signature (Signature.to_b58check signed) ;
                            e_bytes_string "hello world"] in
  let make_expected = e_bool true in
  let%bind () = expect_eq program "check_signature" make_input make_expected in
  ok ()

let check_signature_mligo () : (unit, _) result =
  let open Tezos_crypto in
  let (_, raw_pk, sk) = Signature.generate_key () in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let signed = Signature.sign sk (Bytes.of_string "hello world") in
  let%bind program = mtype_file "./contracts/check_signature.mligo" in
  let make_input = e_tuple [e_key pk_str ;
                            e_signature (Signature.to_b58check signed) ;
                            e_bytes_string "hello world"] in
  let make_expected = e_bool true in
  let%bind () = expect_eq program "check_signature" make_input make_expected in
  let%bind () = expect_eq_evaluate program "example" (e_bool true) in
  ok ()

let check_signature_religo () : (unit, _) result =
  let open Tezos_crypto in
  let (_, raw_pk, sk) = Signature.generate_key () in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let signed = Signature.sign sk (Bytes.of_string "hello world") in
  let%bind program = retype_file "./contracts/check_signature.religo" in
  let make_input = e_tuple [e_key pk_str ;
                            e_signature (Signature.to_b58check signed) ;
                            e_bytes_string "hello world"] in
  let make_expected = e_bool true in
  let%bind () = expect_eq program "check_signature" make_input make_expected in
  ok ()

let curry () : (unit, _) result =
  let%bind program = mtype_file "./contracts/curry.mligo" in
  let%bind () =
    expect_eq program "main" (e_int 2) (e_int 12)
  in
  let%bind () =
    expect_eq program "partial_apply" (e_int 2) (e_int 12)
  in
  ok ()

let set_delegate () : (unit, _) result =
  let open Tezos_crypto in
  let (raw_pkh,_,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let%bind program = type_file "./contracts/set_delegate.ligo" in
  let%bind () = expect_eq program "main" (e_key_hash pkh_str) (e_typed_list [] (t_operation ()))
  in ok ()

let set_delegate_mligo () : (unit, _) result =
  let open Tezos_crypto in
  let (raw_pkh,_,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let%bind program = mtype_file "./contracts/set_delegate.mligo" in
  let%bind () = expect_eq program "main" (e_key_hash pkh_str) (e_typed_list [] (t_operation ()))
  in ok ()

let set_delegate_religo () : (unit, _) result =
  let open Tezos_crypto in
  let (raw_pkh,_,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let%bind program = retype_file "./contracts/set_delegate.religo" in
  let%bind () = expect_eq program "main" (e_key_hash pkh_str) (e_typed_list [] (t_operation ()))
  in ok ()

let type_tuple_destruct () : (unit, _) result =
  let%bind program = mtype_file "./contracts/type_tuple_destruct.mligo" in
  let%bind () = expect_eq program "type_tuple_d" (e_unit ()) (e_int 35) in
  let%bind () = expect_eq program "type_tuple_d_2" (e_unit ()) (e_string "helloworld") in
  ok ()

let tuple_param_destruct () : (unit, _) result =
  let%bind program = mtype_file "./contracts/tuple_param_destruct.mligo" in
  let%bind () = expect_eq program "sum" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  let%bind () = expect_eq program "parentheses" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  ok ()

let tuple_param_destruct_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/tuple_param_destruct.religo" in
  let%bind () = expect_eq program "sum" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  let%bind () = expect_eq program "parentheses" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  ok ()

let let_in_multi_bind () : (unit, _) result =
  let%bind program = mtype_file "./contracts/let_in_multi_bind.mligo" in
  let%bind () = expect_eq program "sum" (e_tuple [e_int 10; e_int 10]) (e_int 20) in
  let%bind () = expect_eq program "sum2"
      (e_tuple
         [e_string "my" ;
          e_string "name" ;
          e_string "is" ;
          e_string "bob" ])
      (e_string "mynameisbob")
  in ok ()

let bytes_unpack () : (unit, _) result =
  let%bind program = type_file "./contracts/bytes_unpack.ligo" in
  let%bind () = expect_eq program "id_string" (e_string "teststring") (e_some (e_string "teststring")) in
  let%bind () = expect_eq program "id_int" (e_int 42) (e_some (e_int 42)) in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth dummy_environment.identities 0).implicit_contract in
  let%bind () = expect_eq program "id_address" (e_address addr) (e_some (e_address addr)) in
  ok ()

let bytes_unpack_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/bytes_unpack.mligo" in
  let%bind () = expect_eq program "id_string" (e_string "teststring") (e_some (e_string "teststring")) in
  let%bind () = expect_eq program "id_int" (e_int 42) (e_some (e_int 42)) in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth dummy_environment.identities 0).implicit_contract in
  let%bind () = expect_eq program "id_address" (e_address addr) (e_some (e_address addr)) in
  ok ()

let bytes_unpack_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/bytes_unpack.religo" in
  let%bind () = expect_eq program "id_string" (e_string "teststring") (e_some (e_string "teststring")) in
  let%bind () = expect_eq program "id_int" (e_int 42) (e_some (e_int 42)) in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth dummy_environment.identities 0).implicit_contract in
  let%bind () = expect_eq program "id_address" (e_address addr) (e_some (e_address addr)) in
  ok ()

let empty_case () : (unit, _) result =
  let%bind program = type_file "./contracts/empty_case.ligo" in
  let%bind () =
    let input _ = e_constructor "Bar" (e_int 1) in
    let expected _ = e_int 1 in
    expect_eq_n program "main" input expected
  in
  let%bind () =
    let input _ = e_constructor "Baz" (e_unit ()) in
    let expected _ = e_int (-1) in
    expect_eq_n program "main" input expected
  in
  ok ()

let empty_case_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/empty_case.mligo" in
  let%bind () =
    let input _ = e_constructor "Bar" (e_int 1) in
    let expected _ = e_int 1 in
    expect_eq_n program "main" input expected
  in
  let%bind () =
    let input _ = e_constructor "Baz" (e_unit ()) in
    let expected _ = e_int (-1) in
    expect_eq_n program "main" input expected
  in
  ok ()

let empty_case_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/empty_case.religo" in
  let%bind () =
    let input _ = e_constructor "Bar" (e_int 1) in
    let expected _ = e_int 1 in
    expect_eq_n program "main" input expected
  in
  let%bind () =
    let input _ = e_constructor "Baz" (e_unit ()) in
    let expected _ = e_int (-1) in
    expect_eq_n program "main" input expected
  in
  ok ()

let tuple_type_mligo () : (unit, _) result =
  let%bind program = mtype_file "./contracts/tuple_type.mligo" in
  let%bind () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n program "test1" input expected
  in
  let%bind () =
    let input _ = e_int 0 in
    let expected _ = e_int 12 in
    expect_eq_n program "test2" input expected
  in
  ok ()

let tuple_type_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/tuple_type.religo" in
  let%bind () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n program "arguments_test" input expected
  in
  let%bind () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n program "tuple_test" input expected
  in
  let%bind () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n program "arguments_test_inline" input expected
  in
  let%bind () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n program "tuple_test_inline" input expected
  in
  ok ()

let no_semicolon_religo () : (unit, _) result =
  let%bind program = retype_file "./contracts/no_semicolon.religo" in
  let%bind () =
    let input _ = e_int 2 in
    let expected _ = e_int 3 in
    expect_eq_n program "a" input expected
  in
  ok ()

let tuple_list_religo () : (unit, _) result =
  let%bind _ = retype_file "./contracts/tuple_list.religo" in
  ok ()

let single_record_expr_religo () : (unit, _) result =
  let%bind _ = retype_file "./contracts/single_record_item.religo" in
  ok ()

let loop_bugs_ligo () : (unit, _) result =
  let%bind program = type_file "./contracts/loop_bugs.ligo" in
  let input = e_unit () in
  let%bind () =
    let expected = e_string "tata" in
    expect_eq program "shadowing_in_body" input expected in
  let%bind () =
    let expected = e_string "toto" in
    expect_eq program "shadowing_assigned_in_body" input expected in
  ok ()

let test enabled_for_typer_not_currently_in_use name f = enabled_for_typer_not_currently_in_use, test name f
let no = false
let y = true
let main = test_suite "Integration (End to End)"
    @@ (fun lst -> List.map snd @@ match typer_switch () with Ast_typed.New -> List.filter fst lst | _ -> lst) @@ [

    test no "chain id" chain_id ;                         (* record *)
    test no "bytes unpack" bytes_unpack ;                 (* record *)
    test no "bytes unpack (mligo)" bytes_unpack_mligo ;   (* record *)
    test no "bytes unpack (religo)" bytes_unpack_religo ; (* record *)
    test no "key hash" key_hash ;                         (* C_access_label *)
    test no "key hash (mligo)" key_hash_mligo ;           (* C_access_label *)
    test no "key hash (religo)" key_hash_religo ;         (* C_access_label *)
    test no "check signature" check_signature ;                 (* C_access_label *)
    test no "check signature (mligo)" check_signature_mligo ;   (* C_access_label *)
    test no "check signature (religo)" check_signature_religo ; (* C_access_label *)

    test y "type alias" type_alias ;


    test y "function" function_ ;                        (* tests don't typecheck the test case's application *)


    test no "blockless function" blockless;
    (* test "procedure"  procedure ; *)
    test no "assign" assign ;
    test no "declaration local" declaration_local ;
    test no "complex function" complex_function ;
    test y "anon function" anon_function ;
    test y "various applications" application ;

    test no "closure" closure ;
    test no "closure (mligo)" closure_mligo ;
    test no "closure (religo)" closure_religo ;
    test no "shared function" shared_function ;
    test no "shared function (mligo)" shared_function_mligo ;
    test no "shared function (religo)" shared_function_religo ;
    test no "higher order" higher_order ;
    test no "higher order (mligo)" higher_order_mligo ;
    test no "higher order (religo)" higher_order_religo ;
    test y "variant" variant ;
    test y "variant (mligo)" variant_mligo ;
    test y "variant (religo)" variant_religo ;

    test no "variant matching" variant_matching ;
    test no "tuple" tuple ;
    test no "tuple (mligo)" tuple_mligo ;
    test no "tuple (religo)" tuple_religo ;
    test no "record" record ;
    test no "record (mligo)" record_mligo ;
    test no "record (religo)" record_religo ;
    test no "condition simple" condition_simple ;
    test no "condition (ligo)" condition ;
    test no "condition (mligo)" condition_mligo ;
    test no "condition (religo)" condition_religo ;
    test no "sequence (mligo" sequence_mligo ;
    test no "eq bool (ligo)" eq_bool ;
    test no "eq bool (mligo)" eq_bool_mligo ;
    test no "eq bool (religo)" eq_bool_religo ;
    test no "shadow" shadow ;
    test y "annotation" annotation ;

    test no "multiple parameters" multiple_parameters ;
    test no "multiple parameters (mligo)" multiple_parameters_mligo ;
    test no "multiple parameters (religo)" multiple_parameters_religo ;
    test no "bool" bool_expression ;
    test no "bool (mligo)" bool_expression_mligo ;
    test no "bool (religo)" bool_expression_religo ;
    test no "arithmetic" arithmetic ;
    test no "arithmetic (mligo)" arithmetic_mligo ;
    test no "arithmetic (religo)" arithmetic_religo ;
    test no "bitwise_arithmetic" bitwise_arithmetic ;
    test no "bitwise_arithmetic (mligo)" bitwise_arithmetic_mligo;
    test no "bitwise_arithmetic (religo)" bitwise_arithmetic_religo;
    test no "string_arithmetic" string_arithmetic ;
    test no "string_arithmetic (mligo)" string_arithmetic_mligo ;
    test no "string_arithmetic (religo)" string_arithmetic_religo ;
    test no "bytes_arithmetic" bytes_arithmetic ;
    test no "bytes_arithmetic (mligo)" bytes_arithmetic_mligo ;
    test no "bytes_arithmetic (religo)" bytes_arithmetic_religo ;
    test no "comparable (mligo)" comparable_mligo;
    test no "crypto" crypto ;
    test no "crypto (mligo)" crypto_mligo ;
    test no "crypto (religo)" crypto_religo ;
    (* test "set_arithmetic" set_arithmetic ; *)
    test no "set_arithmetic (mligo)" set_arithmetic_mligo ;
    test no "set_arithmetic (religo)" set_arithmetic_religo ;
    test y "unit" unit_expression ;
    test y "string" string_expression ;
    test no "option" option ;
    test y "option (mligo)" moption ;
    test y "option (religo)" reoption ;

    test no "map" map ;
    test no "map (mligo)" mmap ;
    (* test "map (religo)" remap ; *)
    test no "big_map" big_map ;
    test no "big_map (mligo)" mbig_map ;
    test no "big_map (religo)" rebig_map ;
    test no "list" list ;
    test no "loop" loop ;
    test no "loop (mligo)" loop_mligo ;
    test no "loop (religo)" loop_religo ;
    test no "matching" matching ;
    test no "declarations" declarations ;
    test no "quote declaration" quote_declaration ;
    test no "quote declarations" quote_declarations ;

    test y "#include directives" include_ ;
    test y "#include directives (mligo)" include_mligo ;
    test y "#include directives (religo)" include_religo ;


    test no "counter contract" counter_contract ;
    test no "super counter contract" super_counter_contract ;
    test no "super counter contract" super_counter_contract_mligo ;
    test no "super counter contract (reasonligo)" super_counter_contract_religo ;
    test no "dispatch counter contract" dispatch_counter_contract ;
    test y "basic (mligo)" basic_mligo ;
    test y "basic (religo)" basic_religo ;

    test no "counter contract (mligo)" counter_mligo ;
    test no "counter contract (religo)" counter_religo ;
    test no "let-in (mligo)" let_in_mligo ;
    test no "let-in (religo)" let_in_religo ;
    test no "local type declaration (ligo)" local_type_decl_ligo;
    test no "local type declaration (mligo)" local_type_decl_mligo;
    test no "local type declaration (religo)" local_type_decl_religo;
    test no "match variant (mligo)" match_variant ;
    test no "match variant (religo)" match_variant_re ;
    test no "match variant 2 (mligo)" match_matej ;
    test no "match variant 2 (religo)" match_matej_re ;
    test no "list matching (mligo)" mligo_list ;
    test no "list matching (religo)" religo_list ;
    test no "failwith ligo" failwith_ligo ;
    test no "failwith mligo" failwith_mligo ;
    test no "assert mligo" assert_mligo ;
    test no "recursion (ligo)" recursion_ligo ;
    test no "recursion (mligo)" recursion_mligo ;
    test no "recursion (religo)" recursion_religo ;
    (* test "guess string mligo" guess_string_mligo ; WIP? *)
    test no "lambda mligo" lambda_mligo ;
    test no "lambda religo" lambda_religo ;
    test no "lambda ligo" lambda_ligo ;
    test y "tez (ligo)" tez_ligo ;
    test y "tez (mligo)" tez_mligo ;

    test no "lambda2 mligo" lambda2_mligo ;
    test no "lambda2 religo" lambda2_religo ;
    (* test "fibo (mligo)" fibo_mligo ; *)
    (* test "fibo2 (mligo)" fibo2_mligo ; *)
    (* test "fibo3 (mligo)" fibo3_mligo ; *)
    (* test "fibo4 (mligo)" fibo4_mligo ; *)
    test no "michelson inserion ligo" michelson_insertion_ligo;
    test no "michelson inserion mligo" michelson_insertion_mligo;
    test no "michelson inserion religo" michelson_insertion_religo;
    test no "website1 ligo" website1_ligo ;
    test no "website2 ligo" website2_ligo ;
    test no "website2 (mligo)" website2_mligo ;
    test no "website2 (religo)" website2_religo ;
    test no "let multiple (mligo)" mligo_let_multiple ;
    test no "let multiple (religo)" religo_let_multiple ;
    test no "balance constant" balance_constant ;
    test no "balance constant (mligo)" balance_constant_mligo ;
    test no "balance constant (religo)" balance_constant_religo ;
    test no "amount" amount ;
    test no "amount (mligo)" amount_mligo ;
    test no "amount (religo)" amount_religo ;
    test no "address" address ;
    test no "address (mligo)" address_mligo ;
    test no "address (religo)" address_religo ;
    test y "self address" self_address ;
    test y "self address (mligo)" self_address_mligo ;
    test y "self address (religo)" self_address_religo ;
    test y "implicit account" implicit_account ;
    test y "implicit account (mligo)" implicit_account_mligo ;
    test y "implicit account (religo)" implicit_account_religo ;

    test no "set delegate" set_delegate ;
    test no "set delegate (mligo)" set_delegate_mligo ;
    test no "set delegate (religo)" set_delegate_religo ;
    test no "is_nat" is_nat ;
    test no "is_nat (mligo)" is_nat_mligo ;
    test no "is_nat (religo)" is_nat_religo ;
    test y "tuples_sequences_functions (religo)" tuples_sequences_functions_religo ;

    test no "simple_access (ligo)" simple_access_ligo;
    test no "deep_access (ligo)" deep_access_ligo;
    test no "get_contract (ligo)" get_contract_ligo;
    test y "entrypoints (ligo)" entrypoints_ligo ;

    test no "curry (mligo)" curry ;
    test no "type tuple destruct (mligo)" type_tuple_destruct ;
    test no "attributes (ligo)" attributes_ligo;
    test no "attributes (mligo)" attributes_mligo;
    test no "attributes (religo)" attributes_religo;
    test no "let in multi-bind (mligo)" let_in_multi_bind ;
    test no "tuple param destruct (mligo)" tuple_param_destruct ;
    test no "tuple param destruct (religo)" tuple_param_destruct_religo ;
    test no "empty case" empty_case ;
    test no "empty case (mligo)" empty_case_mligo ;
    test no "empty case (religo)" empty_case_religo ;
    test no "tuple type (mligo)" tuple_type_mligo ;
    test no "tuple type (religo)" tuple_type_religo ;
    test no "no semicolon (religo)" no_semicolon_religo ;
    test no "loop_bugs (ligo)" loop_bugs_ligo ;
    test y "tuple_list (religo)" tuple_list_religo ;
    test y "single_record_expr (religo)" single_record_expr_religo ;
  ]
