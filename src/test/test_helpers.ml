(* code quality: medium 2021-05-05 *)

open Trace
open Main_errors

type test_case = unit Alcotest.test_case
type test =
  | Test_suite of (string * test list)
  | Test of test_case

let options = Compiler_options.make ~infer:true ()

let test_format : 'a Simple_utils.Display.format = {
  (* do not display anything if test succeed *)
  pp = (fun ~display_format _ _ -> ignore display_format; ()) ;
  to_json = (fun _ -> (`Null:Display.json)) ;
}

let wrap_test name f =
  let result =
    trace (test_tracer name) @@
    f () in
  match to_stdlib_result result with
  | Ok ((), annotations) -> ignore annotations; ()
  | Error _ ->
     let format = Display.bind_format test_format Formatter.error_format in
     let disp = Simple_utils.Display.Displayable {value=result ; format} in
     let s = Simple_utils.Display.convert ~display_format:(Dev) disp in
     Format.printf "%s\n" s ;
     raise Alcotest.Test_error

let test name f =
  Test (
    Alcotest.test_case name `Quick @@ fun () ->
    wrap_test name f
  )

let test_suite name lst = Test_suite (name , lst)

let rec test_height : test -> int = fun t ->
  match t with
  | Test _ -> 1
  | Test_suite (_ , lst) -> (List.fold_left ~f:max ~init:1 @@ List.map ~f:test_height lst) + 1

let extract_test : test -> test_case = fun t ->
  match t with
  | Test tc -> tc
  | _ -> assert false

let extract_param : test -> (string * (string * test_case list) list) =
  let extract_element = extract_test in
  let extract_group : test -> (string * test_case list) = fun t ->
    match t with
    | Test tc -> ("isolated" , [ tc ])
    | Test_suite (name , lst) -> (name , List.map ~f:extract_element lst) in
  fun t ->
      match t with
      | Test tc -> ("" , [ ("isolated" , [ tc ] ) ])
      | Test_suite (name , lst) -> (name , List.map ~f:extract_group lst)

let rec run_test ?(prefix = "") : test -> unit = fun t ->
  match t with
  | Test case -> Alcotest.run "isolated test" [ ("" , [ case ]) ]
  | Test_suite (name , lst) -> (
      if (test_height t <= 3) then (
        let (name , tests) = extract_param t in
        Alcotest.run (prefix ^ name) tests
      ) else (
        List.iter ~f: (run_test ~prefix:(prefix ^ name ^ "_")) lst
      )
    )

let wrap_ref f =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> f s

(* Common functions used in tests *)

let type_file ?(st = "auto") f entry options =
  Ligo_compile.Utils.type_file ~options f st entry

let get_program ?(st = "auto") f entry =
  wrap_ref (fun s ->
      let options = Compiler_options.make () in
      let* program = type_file ~st f entry options in
      s := Some program ;
      ok program
    )

let expression_to_core expression =
  let* sugar = Ligo_compile.Of_imperative.compile_expression expression in
  let* core  = Ligo_compile.Of_sugar.compile_expression sugar in
  ok @@ core

let pack_payload (env:Ast_typed.environment) (payload:Ast_imperative.expression) : (bytes,_) result =
  let* code =
    let* sugar     = Ligo_compile.Of_imperative.compile_expression payload in
    let* core      = Ligo_compile.Of_sugar.compile_expression sugar in
    let* typed,_ = Ligo_compile.Of_core.compile_expression ~infer:options.infer ~env core in
    let* mini_c = Ligo_compile.Of_typed.compile_expression typed in
    Ligo_compile.Of_mini_c.compile_expression ~options mini_c in
  let payload_ty = code.expr_ty in
  let* (payload : _ Tezos_utils.Michelson.michelson) =
    Run.Of_michelson.evaluate_expression code.expr code.expr_ty in
  Run.Of_michelson.pack_payload payload payload_ty

let sign_message (env:Ast_typed.environment) (payload : Ast_imperative.expression) sk : (string,_) result =
  let open Tezos_crypto in
  let* packed_payload = pack_payload env payload in
  let signed_data = Signature.sign sk packed_payload in
  let signature_str = Signature.to_b58check signed_data in
  ok signature_str

let contract id =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn dummy_environment.identities id in
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

let typed_program_to_michelson (program, env) entry_point =
  ignore env;
  let* mini_c = Ligo_compile.Of_typed.compile program in
  let* michelson = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~options mini_c entry_point in
  let* michelson = Ligo_compile.Of_michelson.build_contract ~disable_typecheck:false michelson in
  ok michelson

let typed_program_with_imperative_input_to_michelson ((program , env): Ast_typed.module_fully_typed * Ast_typed.environment) (entry_point: string) (input: Ast_imperative.expression) : (Stacking.compiled_expression,_) result =
  Printexc.record_backtrace true;
  let* sugar            = Ligo_compile.Of_imperative.compile_expression input in
  let* core             = Ligo_compile.Of_sugar.compile_expression sugar in
  let* app              = Ligo_compile.Of_core.apply entry_point core in
  let* (typed_app,_env) = Ligo_compile.Of_core.compile_expression ~infer:options.infer ~env app in
  let* compiled_applied = Ligo_compile.Of_typed.compile_expression typed_app in
  let* mini_c_prg       = Ligo_compile.Of_typed.compile program in
  Ligo_compile.Of_mini_c.aggregate_and_compile_expression ~options mini_c_prg compiled_applied

let run_typed_program_with_imperative_input ?options ((program, env): Ast_typed.module_fully_typed * Ast_typed.environment ) (entry_point: string) (input: Ast_imperative.expression) : (Ast_core.expression, _) result =
  let* michelson_program = typed_program_with_imperative_input_to_michelson (program, env) entry_point input in
  let* michelson_output  = Run.Of_michelson.run_no_failwith ?options michelson_program.expr michelson_program.expr_ty in
  let* res =  Decompile.Of_michelson.decompile_typed_program_entry_function_result program entry_point (Runned_result.Success michelson_output) in
  match res with
  | Runned_result.Success exp -> ok exp
  | Runned_result.Fail _ -> fail test_not_expected_to_fail

let expect ?options program entry_point input expecter =
  let* result =
    trace (test_run_tracer entry_point) @@
    run_typed_program_with_imperative_input ?options program entry_point input in
  expecter result

let expect_fail ?options program entry_point input =
  trace (test_run_tracer entry_point) @@
    Assert.assert_fail (test_expected_to_fail) @@
    run_typed_program_with_imperative_input ?options program entry_point input

let expect_string_failwith ?options program entry_point input expected_failwith =
  let* michelson_program = typed_program_with_imperative_input_to_michelson program entry_point input in
  let* err = Run.Of_michelson.run_failwith
    ?options michelson_program.expr michelson_program.expr_ty in
  match err with
    | Runned_result.Failwith_string s when String.equal s expected_failwith -> ok ()
    | _ -> fail test_expected_to_fail

let expect_eq ?options program entry_point input expected =
  let* expected = expression_to_core expected in
  let expecter = fun result ->
    trace_option (test_expect expected result) @@
    Ast_core.Misc.assert_value_eq (expected,result) in
  expect ?options program entry_point input expecter

let expect_eq_core ?options program entry_point input expected =
  let expecter = fun result ->
    trace_option (test_expect expected result) @@
    Ast_core.Misc.assert_value_eq (expected,result) in
  expect ?options program entry_point input expecter

let expect_evaluate (program, _env) entry_point expecter =
  trace (test_run_tracer entry_point) @@
  let* mini_c          = Ligo_compile.Of_typed.compile program in
  let* (exp,_)         = trace_option unknown @@ Mini_c.get_entry mini_c entry_point in
  let* michelson_value = Ligo_compile.Of_mini_c.aggregate_and_compile_expression ~options mini_c exp in
  let* res_michelson   = Run.Of_michelson.run_no_failwith michelson_value.expr michelson_value.expr_ty in
  let* res             = Decompile.Of_michelson.decompile_typed_program_entry_expression_result program entry_point (Success res_michelson) in
  let* res' = match res with
  | Runned_result.Success exp -> ok exp
  | Runned_result.Fail _ -> fail test_not_expected_to_fail in
  expecter res'

let expect_eq_evaluate ((program , env) : Ast_typed.module_fully_typed * Ast_typed.environment) entry_point expected =
  let* expected  = expression_to_core expected in
  let expecter = fun result ->
    trace_option (test_expect expected result) @@
    Ast_core.Misc.assert_value_eq (expected , result) in
  expect_evaluate (program, env) entry_point expecter

let expect_n_aux ?options lst program entry_point make_input make_expecter =
  let aux n =
    let input = make_input n in
    let expecter = make_expecter n in
    trace (test_expect_n_tracer n) @@
    let result = expect ?options program entry_point input expecter in
    result
  in
  let* _ = bind_map_list aux lst in
  ok ()

let expect_eq_n_trace_aux ?options lst program entry_point make_input make_expected =
  let aux n =
    let* input = make_input n in
    let* expected = make_expected n in
    trace (test_expect_n_tracer n) @@
    let result = expect_eq ?options program entry_point input expected in
    result
  in
  let* _ = bind_map_list_seq aux lst in
  ok ()

let expect_eq_exp_trace_aux ?options explst program entry_point make_input make_expected =
  let aux exp =
    let* input = make_input exp in
    let* expected = make_expected exp in
    trace (test_expect_exp_tracer exp) @@
    let result = expect_eq ?options program entry_point input expected in
    result
  in
  let* _ = bind_map_list_seq aux explst in
  ok ()

let expect_failwith_exp_trace_aux ?options explst program entry_point make_input make_expected_failwith =
  let aux exp =
    let* input = make_input exp in
    let* expected = make_expected_failwith exp in
    trace (test_expect_exp_tracer exp) @@
    let result = expect_string_failwith ?options program entry_point input expected in
    result
  in
  let* _ = bind_map_list_seq aux explst in
  ok ()

let expect_eq_n_aux ?options lst program entry_point make_input make_expected =
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    trace (test_expect_eq_n_tracer n) @@
    let result = expect_eq ?options program entry_point input expected in
    result
  in
  let* _ = bind_map_list_seq aux lst in
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
  let open Ast_imperative in
  let aux b =
    let input = e_bool b in
    let expected = make_expected b in
    expect_eq program entry_point input expected
  in
  let* _ = bind_map_list_seq aux [false ; true] in
  ok ()

let expect_eq_n_int a b c =
  let open Ast_imperative in
  expect_eq_n a b e_int (fun n -> e_int (c n))

let expect_eq_b_bool a b c =
  let open Ast_imperative in
  expect_eq_b a b (fun bool -> e_bool (c bool))
