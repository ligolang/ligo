(* code quality: medium 2021-05-05 *)
module Display = Simple_utils.Display
module Runned_result = Simple_utils.Runned_result
open Simple_utils.Trace
open Main_errors

type test_case = unit Alcotest.test_case
type test =
  | Test_suite of (string * test list)
  | Test of test_case

let options =
  Compiler_options.make
    ~raw_options:Compiler_options.default_raw_options
    ~protocol_version:Environment.Protocols.in_use ()

let test_format : 'a Simple_utils.Display.format = {
  (* do not display anything if test succeed *)
  pp = (fun ~display_format _ _ -> ignore display_format; ()) ;
  to_json = (fun _ -> (`Null:Display.json)) ;
}

let wrap_test_w name f =
  try_with (fun ~raise ->
  let () =
    f ~raise () in
    List.iter ~f:(fun w ->
      Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev) w ;
    ) @@ raise.get_warnings () ;
  )
  (fun ~raise error ->
    let value = Error (test_err_tracer name error) in
     let format = Display.bind_format test_format Formatter.error_format in
     let disp = Simple_utils.Display.Displayable {value ; format} in
     let s = Simple_utils.Display.convert ~display_format:(Dev) disp in
    List.iter ~f:(fun w ->
      Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev) w ;
    ) @@ raise.get_warnings () ;
     Format.printf "%s\n" s ;
     Stdlib.raise Alcotest.Test_error
  )

let test_w name test =
  Test (
    Alcotest.test_case name `Quick @@ fun () ->
    wrap_test_w name test
  )

let test_w_all name test =
  List.map ~f:(fun s ->
  let file = "./contracts/" ^ Str.(global_replace (regexp " ") "_" name) ^ "." ^ s in
  let name = Format.asprintf "%s (%s)" name s in
  let f ~raise () = test ~raise file in
  test_w name f
  ) ["ligo";"mligo";"religo";"jsligo"]

let wrap_test name f =
    try_with (fun ~raise -> f ~raise ())
    (fun ~raise:_ error ->
    let value = Error (test_err_tracer name error) in
     let format = Display.bind_format test_format Formatter.error_format in
     let disp = Simple_utils.Display.Displayable {value ; format} in
     let s = Simple_utils.Display.convert ~display_format:(Dev) disp in
     Format.printf "%s\n" s ;
     raise Alcotest.Test_error)

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

let wrap_ref file f =
  let s = ref None in
  fun () -> match !s with
    | Some (a,file') ->
      if String.equal file' file then
        a else f s
    | None -> f s

(* Common functions used in tests *)

let type_file ~raise ?(st = "auto") f entry options =
  ignore st;
  snd @@ Build.build_typed ~raise ~options entry f

let get_program ~raise ?(st = "auto") f entry =
  wrap_ref f (fun s ->
      let program = type_file ~raise ~st f entry options in
      s := Some (program,f) ;
      program
    )

let get_program f ?st = get_program ?st f (Contract (Ast_typed.ValueVar.of_input_var "main"))
let expression_to_core ~raise expression =
  let sugar = Ligo_compile.Of_imperative.compile_expression ~raise expression in
  let core  = Ligo_compile.Of_sugar.compile_expression ~raise sugar in
  core

let pack_payload ~raise (program:Ast_typed.program) (payload:Ast_imperative.expression) : bytes =
  let code =
    let sugar     = Ligo_compile.Of_imperative.compile_expression ~raise payload in
    let core      = Ligo_compile.Of_sugar.compile_expression ~raise sugar in
    let typed      = Ligo_compile.Of_core.compile_expression ~raise ~options ~init_prog:program core in
    let aggregated = Ligo_compile.Of_typed.compile_expression ~raise ~options:options.middle_end typed in
    let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  let payload_ty = code.expr_ty in
  let (payload : _ Tezos_utils.Michelson.michelson) =
    Ligo_run.Of_michelson.evaluate_expression ~raise code.expr code.expr_ty in
  Ligo_run.Of_michelson.pack_payload ~raise payload payload_ty

let sign_message ~raise (program:Ast_typed.program) (payload : Ast_imperative.expression) sk : string =
  let open Tezos_crypto in
  let packed_payload = pack_payload ~raise program payload in
  let signed_data = Signature.sign sk packed_payload in
  let signature_str = Signature.to_b58check signed_data in
  signature_str

let contract id =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities id in
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

let typed_program_to_michelson ~raise (program, env) =
  ignore env;
  let aggregated = Ligo_compile.Of_typed.compile_expression ~raise ~options:options.middle_end program in
  let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let michelson = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  let michelson = Ligo_compile.Of_michelson.build_contract ~disable_typecheck:false michelson in
  michelson

let typed_program_with_imperative_input_to_michelson ~raise (program : Ast_typed.program) (entry_point: string) (input: Ast_imperative.expression) : Stacking.compiled_expression *  Ast_aggregated.type_expression =
  Printexc.record_backtrace true;
  let sugar            = Ligo_compile.Of_imperative.compile_expression ~raise input in
  let core             = Ligo_compile.Of_sugar.compile_expression ~raise sugar in
  let app              = Ligo_compile.Of_core.apply entry_point core in
  let typed_app        = Ligo_compile.Of_core.compile_expression ~raise ~options ~init_prog:program app in
  (* let compiled_applied = Ligo_compile.Of_typed.compile_expression ~raise typed_app in *)
  let program = Ligo_compile.Of_typed.compile_program ~raise program in
  let aggregated = Ligo_compile.Of_typed.compile_expression_in_context ~raise ~options:options.middle_end typed_app program in
  let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c, aggregated.type_expression

let run_typed_program_with_imperative_input ~raise ?options (program : Ast_typed.program) (entry_point: string) (input: Ast_imperative.expression) : Ast_core.expression =
  let michelson_program,ty = typed_program_with_imperative_input_to_michelson ~raise program entry_point input in
  let michelson_output  = Ligo_run.Of_michelson.run_no_failwith ~raise ?options michelson_program.expr michelson_program.expr_ty in
  let res =  Decompile.Of_michelson.decompile_expression ~raise ty (Runned_result.Success michelson_output) in
  match res with
  | Runned_result.Success exp -> exp
  | Runned_result.Fail _ -> raise.error test_not_expected_to_fail

let expect ~raise ?options program entry_point input expecter =
  let result =
    trace ~raise (test_run_tracer entry_point) @@
    run_typed_program_with_imperative_input ?options program entry_point input in
  expecter result

let expect_fail ~raise ?options program entry_point input =
  trace ~raise (test_run_tracer entry_point) @@
    fun ~raise -> Assert.assert_fail ~raise (test_expected_to_fail) @@
    run_typed_program_with_imperative_input ?options program entry_point input

let expect_string_failwith ~raise ?options program entry_point input expected_failwith =
  let michelson_program,_ = typed_program_with_imperative_input_to_michelson ~raise program entry_point input in
  let err = Ligo_run.Of_michelson.run_failwith ~raise
    ?options michelson_program.expr michelson_program.expr_ty in
  match err with
  | String (_,str) when String.equal str expected_failwith -> ()
  | _ -> raise.error test_expected_to_fail

let expect_eq ~raise ?options program entry_point input expected =
  let expected = expression_to_core ~raise expected in
  let expecter = fun result ->
    trace_option ~raise (test_expect_tracer expected result) @@
    Ast_core.Misc.assert_value_eq (expected,result) in
  expect ~raise ?options program entry_point input expecter

let expect_eq_core ~raise ?options program entry_point input expected =
  let expecter = fun result ->
    trace_option ~raise (test_expect_tracer expected result) @@
    Ast_core.Misc.assert_value_eq (expected,result) in
  expect ~raise ?options program entry_point input expecter

let expect_evaluate ~raise (program : Ast_typed.program) entry_point expecter =
  trace ~raise (test_run_tracer entry_point) @@
  let aggregated      = Ligo_compile.Of_typed.apply_to_entrypoint ~raise ~options:options.middle_end program entry_point in
  let mini_c          = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let michelson_value = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  let res_michelson   = Ligo_run.Of_michelson.run_no_failwith ~raise michelson_value.expr michelson_value.expr_ty in
  let res             = Decompile.Of_michelson.decompile_expression ~raise aggregated.type_expression (Runned_result.Success res_michelson) in
  let res' = match res with
  | Runned_result.Success exp -> exp
  | Runned_result.Fail _ -> raise.error test_not_expected_to_fail in
  expecter res'

let expect_eq_evaluate ~raise (program : Ast_typed.program) entry_point expected =
  let expected  = expression_to_core ~raise expected in
  let expecter = fun result ~raise ->
    trace_option ~raise (test_expect_tracer expected result) @@
    Ast_core.Misc.assert_value_eq (expected , result) in
  expect_evaluate ~raise program entry_point expecter

let expect_n_aux ~raise ?options lst program entry_point make_input make_expecter =
  let aux n =
    let input = make_input n in
    let expecter = make_expecter n in
    trace ~raise (test_expect_n_tracer n) @@
    let result = expect ?options program entry_point input expecter in
    result
  in
  let _ = List.map ~f:aux lst in
  ()

let expect_eq_n_trace_aux ~raise ?options lst program entry_point make_input make_expected =
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    trace ~raise (test_expect_n_tracer n) @@
      expect_eq ?options program entry_point input expected
  in
  let _ = List.map ~f:aux lst in
  ()

let expect_eq_exp_trace_aux ~raise ?options explst program entry_point make_input make_expected =
  let aux exp =
    let input = make_input exp in
    let expected = make_expected exp in
    trace ~raise (test_expect_exp_tracer exp) @@
      expect_eq ?options program entry_point input expected
  in
  let _ = List.map ~f:aux explst in
  ()

let expect_failwith_exp_trace_aux ~raise ?options explst program entry_point make_input make_expected_failwith =
  let aux exp =
    let input = make_input exp in
    let expected = make_expected_failwith exp in
    trace ~raise (test_expect_exp_tracer exp) @@
      expect_string_failwith ?options program entry_point input expected
  in
  let _ = List.map ~f:aux explst in
  ()

let expect_eq_n_aux ~raise ?options lst program entry_point make_input make_expected =
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    trace ~raise (test_expect_eq_n_tracer n) @@
      expect_eq ?options program entry_point input expected
  in
  let () = List.iter ~f:aux lst in
  ()

let expect_eq_n ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 42 ; 163 ; -1]
let expect_eq_n_pos ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 42 ; 163]
let expect_eq_n_strict_pos ?options = expect_eq_n_aux ?options [2 ; 42 ; 163]
let expect_eq_n_pos_small ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 10]
let expect_eq_n_strict_pos_small ?options = expect_eq_n_aux ?options [1 ; 2 ; 10]
let expect_eq_n_pos_mid = expect_eq_n_aux [0 ; 1 ; 2 ; 10 ; 33]

let expect_n_pos_small ?options = expect_n_aux ?options [0 ; 2 ; 10]
let expect_n_strict_pos_small ?options = expect_n_aux ?options [2 ; 10]

let expect_eq_b ~raise program entry_point make_expected =
  let open Ast_imperative in
  let aux b =
    let input = e_bool b in
    let expected = make_expected b in
    expect_eq ~raise program entry_point input expected
  in
  let () = List.iter ~f:aux [false ; true] in
  ()

let expect_eq_n_int a b c =
  let open Ast_imperative in
  expect_eq_n a b e_int (fun n -> e_int (c n))

let expect_eq_b_bool a b c =
  let open Ast_imperative in
  expect_eq_b a b (fun bool -> e_bool (c bool))

let compile_main ~raise f () =
  let agg = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise ~options:options.middle_end (get_program ~raise f ()) @@ Ast_typed.ValueVar.of_input_var "main" in
  let mini_c    = Ligo_compile.Of_aggregated.compile_expression ~raise agg in
  let michelson_prg = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
  let _contract : _ Tezos_utils.Michelson.michelson =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract ~raise ~protocol_version:Environment.Protocols.in_use michelson_prg [] in
  ()
