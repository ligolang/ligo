open! Trace

type test_case = unit Alcotest.test_case
type test =
  | Test_suite of (string * test list)
  | Test of test_case

let wrap_test name f =
  let result =
    trace (error (thunk "running test") (thunk name)) @@
    f () in
  match result with
  | Ok ((), annotations) -> ignore annotations; ()
  | Error err ->
    Format.printf "%a\n%!" (Ligo.Display.error_pp ~dev:true) (err ()) ;
    raise Alcotest.Test_error

let wrap_test_raw f =
  match f () with
  | Trace.Ok ((), annotations) -> ignore annotations; ()
  | Error err ->
    Format.printf "%a\n%!" (Ligo.Display.error_pp ~dev:true) (err ())

let test name f =
  Test (
    Alcotest.test_case name `Quick @@ fun () ->
    wrap_test name f
  )

let test_suite name lst = Test_suite (name , lst)


open Ast_simplified

let pack_payload (program:Ast_typed.program) (payload:expression) : bytes result =
  let%bind code =
    let env = Ast_typed.program_environment program in
    let%bind (typed,_) = Compile.Of_simplified.compile_expression
        ~env ~state:(Typer.Solver.initial_state) payload in
    let%bind mini_c = Compile.Of_typed.compile_expression typed in
    Compile.Of_mini_c.compile_expression mini_c in
  let (Ex_ty payload_ty) = code.expr_ty in
  let%bind (payload: Tezos_utils.Michelson.michelson) =
    Ligo.Run.Of_michelson.evaluate_expression code.expr code.expr_ty in
  Ligo.Run.Of_michelson.pack_payload payload payload_ty

let sign_message (program:Ast_typed.program) (payload : expression) sk : string result =
  let open Tezos_crypto in
  let%bind packed_payload = pack_payload program payload in
  let signed_data = Signature.sign sk packed_payload in
  let signature_str = Signature.to_b58check signed_data in
  ok signature_str

let contract id =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities id in
  id.implicit_contract

let addr id =
  let open Proto_alpha_utils.Memory_proto_alpha in
  Protocol.Alpha_context.Contract.to_b58check @@ contract id

let gen_keys = fun () ->
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,raw_sk) = Signature.generate_key () in
  (raw_pkh,raw_pk,raw_sk)

let str_keys (raw_pkh, raw_pk, raw_sk) =
  let open Tezos_crypto in
  let sk_str = Signature.Secret_key.to_b58check raw_sk in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  (pkh_str,pk_str,sk_str)

let sha_256_hash pl =
  let open Proto_alpha_utils.Memory_proto_alpha.Alpha_environment in
  Raw_hashes.sha256 pl

open Ast_simplified.Combinators

let typed_program_with_simplified_input_to_michelson
    (program: Ast_typed.program) (entry_point: string)
    (input: Ast_simplified.expression) : (Compiler.compiled_expression * Tezos_utils.Michelson.michelson) result =
  let env = Ast_typed.program_environment program in
  let%bind (typed_in,_)      = Compile.Of_simplified.compile_expression ~env ~state:(Typer.Solver.initial_state) input in
  let%bind mini_c_in         = Compile.Of_typed.compile_expression typed_in in
  (* might be useless *)
  let%bind michelson_in      = Compile.Of_mini_c.compile_expression mini_c_in in
  let%bind evaluated_in      = Ligo.Run.Of_michelson.evaluate_expression michelson_in.expr michelson_in.expr_ty in

  let%bind mini_c_prg = Compile.Of_typed.compile program in
  let%bind michelson_program = Compile.Of_mini_c.aggregate_and_compile mini_c_prg (Some [mini_c_in]) entry_point in
  ok (michelson_program, evaluated_in)

let run_typed_program_with_simplified_input ?options
    (program: Ast_typed.program) (entry_point: string)
    (input: Ast_simplified.expression) : Ast_simplified.expression result =
  let%bind (michelson_program, _evaluated_in) = typed_program_with_simplified_input_to_michelson program entry_point input in
  (* let%bind michelson_output  = Ligo.Run.Of_michelson.run_contract
      ?options michelson_program.expr michelson_program.expr_ty _evaluated_in false in *)
  let%bind michelson_output  = Ligo.Run.Of_michelson.run ?options michelson_program.expr michelson_program.expr_ty in
  Uncompile.uncompile_typed_program_entry_function_result program entry_point michelson_output

let expect ?options program entry_point input expecter =
  let%bind result =
    let run_error =
      let title () = "expect run" in
      let content () = Format.asprintf "Entry_point: %s" entry_point in
      error title content
    in
    trace run_error @@
    run_typed_program_with_simplified_input ?options program entry_point input in

  expecter result

let expect_fail ?options program entry_point input =
  let run_error =
    let title () = "expect run" in
    let content () = Format.asprintf "Entry_point: %s" entry_point in
    error title content
  in
  trace run_error @@
  Assert.assert_fail @@
  run_typed_program_with_simplified_input ?options program entry_point input

let expect_string_failwith ?options program entry_point input expected_failwith =
  let%bind (michelson_program, _evaluated_in) = typed_program_with_simplified_input_to_michelson program entry_point input in
  let%bind err = Ligo.Run.Of_michelson.run_failwith
    ?options michelson_program.expr michelson_program.expr_ty in
  match err with
    | Ligo.Run.Of_michelson.Failwith_string s -> Assert.assert_equal_string expected_failwith s
    | _ -> simple_fail "Expected to fail with a string"

let expect_eq ?options program entry_point input expected =
  let expecter = fun result ->
    let expect_error =
      let title () = "expect result" in
      let content () = Format.asprintf "Expected %a, got %a"
          Ast_simplified.PP.expression expected
          Ast_simplified.PP.expression result in
      error title content in
    trace expect_error @@
    Ast_simplified.Misc.assert_value_eq (expected , result) in
  expect ?options program entry_point input expecter

let expect_evaluate program entry_point expecter =
  let error =
    let title () = "expect evaluate" in
    let content () = Format.asprintf "Entry_point: %s" entry_point in
    error title content in
  trace error @@
  let%bind mini_c = Ligo.Compile.Of_typed.compile program in
  let%bind michelson_value = Ligo.Compile.Of_mini_c.aggregate_and_compile mini_c (Some []) entry_point in
  let%bind res_michelson = Ligo.Run.Of_michelson.run michelson_value.expr michelson_value.expr_ty in
  let%bind res_simpl = Uncompile.uncompile_typed_program_entry_expression_result program entry_point res_michelson in
  expecter res_simpl

let expect_eq_evaluate program entry_point expected =
  let expecter = fun result ->
    Ast_simplified.Misc.assert_value_eq (expected , result) in
  expect_evaluate program entry_point expecter

let expect_n_aux ?options lst program entry_point make_input make_expecter =
  let aux n =
    let input = make_input n in
    let expecter = make_expecter n in
    trace (simple_error ("expect_n " ^ (string_of_int n))) @@
    let result = expect ?options program entry_point input expecter in
    result
  in
  let%bind _ = bind_map_list aux lst in
  ok ()

let expect_eq_n_trace_aux ?options lst program entry_point make_input make_expected =
  let aux n =
    let%bind input = make_input n in
    let%bind expected = make_expected n in
    trace (simple_error ("expect_eq_n " ^ (string_of_int n))) @@
    let result = expect_eq ?options program entry_point input expected in
    result
  in
  let%bind _ = bind_map_list_seq aux lst in
  ok ()

let expect_eq_exp_trace_aux ?options explst program entry_point make_input make_expected =
  let aux exp =
    let%bind input = make_input exp in
    let%bind expected = make_expected exp in
    let pps = Format.asprintf "%a" Ast_simplified.PP.expression exp in
    trace (simple_error ("expect_eq_exp " ^ pps )) @@
    let result = expect_eq ?options program entry_point input expected in
    result
  in
  let%bind _ = bind_map_list_seq aux explst in
  ok ()

let expect_failwith_exp_trace_aux ?options explst program entry_point make_input make_expected_failwith =
  let aux exp =
    let%bind input = make_input exp in
    let%bind expected = make_expected_failwith exp in
    let pps = Format.asprintf "%a" Ast_simplified.PP.expression exp in
    trace (simple_error ("expect_eq_exp " ^ pps )) @@
    let result = expect_string_failwith ?options program entry_point input expected in
    result
  in
  let%bind _ = bind_map_list_seq aux explst in
  ok ()

let expect_eq_n_aux ?options lst program entry_point make_input make_expected =
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    trace (simple_error ("expect_eq_n " ^ (string_of_int n))) @@
    let result = expect_eq ?options program entry_point input expected in
    result
  in
  let%bind _ = bind_map_list_seq aux lst in
  ok ()

let expect_eq_n ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 42 ; 163 ; -1]
let expect_eq_n_pos ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 42 ; 163]
let expect_eq_n_strict_pos ?options = expect_eq_n_aux ?options [2 ; 42 ; 163]
let expect_eq_n_pos_small ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 10]
let expect_eq_n_strict_pos_small ?options = expect_eq_n_aux ?options [1 ; 2 ; 10]
let expect_eq_n_pos_mid = expect_eq_n_aux [0 ; 1 ; 2 ; 10 ; 33]

let expect_n_pos_small ?options = expect_n_aux ?options [0 ; 2 ; 10]
let expect_n_strict_pos_small ?options = expect_n_aux ?options [2 ; 10]

let expect_eq_b program entry_point make_expected =
  let aux b =
    let input = e_bool b in
    let expected = make_expected b in
    expect_eq program entry_point input expected
  in
  let%bind _ = bind_map_list_seq aux [false ; true] in
  ok ()

let expect_eq_n_int a b c =
  expect_eq_n a b e_int (fun n -> e_int (c n))

let expect_eq_b_bool a b c =
  let open Ast_simplified.Combinators in
  expect_eq_b a b (fun bool -> e_bool (c bool))


let rec test_height : test -> int = fun t ->
  match t with
  | Test _ -> 1
  | Test_suite (_ , lst) -> (List.fold_left max 1 @@ List.map test_height lst) + 1

let extract_test : test -> test_case = fun t ->
  match t with
  | Test tc -> tc
  | _ -> assert false

let extract_param : test -> (string * (string * test_case list) list) =
  let extract_element = extract_test in
  let extract_group : test -> (string * test_case list) = fun t ->
    match t with
    | Test tc -> ("isolated" , [ tc ])
    | Test_suite (name , lst) -> (name , List.map extract_element lst) in
  fun t ->
      match t with
      | Test tc -> ("" , [ ("isolated" , [ tc ] ) ])
      | Test_suite (name , lst) -> (name , List.map extract_group lst)

let x : _ -> (unit Alcotest.test) = fun x -> x

(*
  Alcotest.run parameters:
  string * (string * f list) list
*)

let rec run_test ?(prefix = "") : test -> unit = fun t ->
  match t with
  | Test case -> Alcotest.run "isolated test" [ ("" , [ case ]) ]
  | Test_suite (name , lst) -> (
      if (test_height t <= 3) then (
        let (name , tests) = extract_param t in
        Alcotest.run (prefix ^ name) tests
      ) else (
        List.iter (run_test ~prefix:(prefix ^ name ^ "_")) lst
      )
    )
