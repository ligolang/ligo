(* code quality: medium 2021-05-05 *)
module Display = Simple_utils.Display
module Runned_result = Simple_utils.Runned_result
open Simple_utils.Trace
open Main_errors
module Raw_options = Compiler_options.Raw_options

let make_options
    ?tezos_context
    ?constants
    ?now
    ?sender
    ?self
    ?parameter_ty
    ?source
    ?amount
    ?balance
    ?chain_id
    ()
  =
  Lwt_main.run
    Proto_alpha_utils.Memory_proto_alpha.(
      Lwt.bind (test_environment ()) (fun env ->
          make_options
            ~env
            ?tezos_context
            ?constants
            ?now
            ?sender
            ?self
            ?parameter_ty
            ?source
            ?amount
            ?balance
            ?chain_id
            ()))


type test_case = unit Alcotest.test_case

type test =
  | Test_suite of (string * test list)
  | Test of test_case

let options =
  Compiler_options.make
    ~raw_options:(Raw_options.make ())
    ()


let loc = Location.test

let test_format : 'a Simple_utils.Display.format =
  { (* do not display anything if test succeed *)
    pp =
      (fun ~display_format ~no_colour _ _ ->
        ignore display_format;
        ignore no_colour;
        ())
  ; to_json = (fun _ : Display.json -> `Null)
  }


let wrap_test_w ~no_colour name f =
  try_with
    (fun ~raise ~catch ->
      let () = f ~raise () in
      List.iter ~f:(fun w ->
          Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev ~no_colour) w)
      @@ catch.warnings ())
    (fun ~catch error ->
      let value = Error (test_err_tracer name error) in
      let format = Display.bind_format test_format Formatter.error_format in
      let disp = Simple_utils.Display.Displayable { value; format } in
      let s = Simple_utils.Display.convert ~display_format:Dev ~no_colour disp in
      List.iter ~f:(fun w ->
          Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev ~no_colour) w)
      @@ catch.warnings ();
      Format.printf "%s\n" s;
      Stdlib.raise Alcotest.Test_error)


let test_w name test =
  Test
    (Alcotest.test_case name `Quick
    @@ fun () -> wrap_test_w ~no_colour:options.tools.no_colour name test)


let test_w_all name test =
  List.map
    ~f:(fun s ->
      let file = "./contracts/" ^ Str.(global_replace (regexp " ") "_" name) ^ "." ^ s in
      let name = Format.asprintf "%s (%s)" name s in
      let f ~raise () = test ~raise file in
      test_w name f)
    [ "mligo"; "jsligo" ]


let wrap_test ~no_colour name f =
  try_with
    (fun ~raise ~catch:_ -> f ~raise ())
    (fun ~catch:_ error ->
      let value = Error (test_err_tracer name error) in
      let format = Display.bind_format test_format Formatter.error_format in
      let disp = Simple_utils.Display.Displayable { value; format } in
      let s = Simple_utils.Display.convert ~display_format:Dev ~no_colour disp in
      Format.printf "%s\n" s;
      raise Alcotest.Test_error)


let test name f =
  Test
    (Alcotest.test_case name `Quick
    @@ fun () -> wrap_test ~no_colour:options.tools.no_colour name f)


let test_suite name lst = Test_suite (name, lst)

let rec test_height : test -> int =
 fun t ->
  match t with
  | Test _ -> 1
  | Test_suite (_, lst) ->
    (List.fold_left ~f:max ~init:1 @@ List.map ~f:test_height lst) + 1


let extract_test : test -> test_case =
 fun t ->
  match t with
  | Test tc -> tc
  | _ -> assert false


let extract_param : test -> string * (string * test_case list) list =
  let extract_element = extract_test in
  let extract_group : test -> string * test_case list =
   fun t ->
    match t with
    | Test tc -> "isolated", [ tc ]
    | Test_suite (name, lst) -> name, List.map ~f:extract_element lst
  in
  fun t ->
    match t with
    | Test tc -> "", [ "isolated", [ tc ] ]
    | Test_suite (name, lst) -> name, List.map ~f:extract_group lst


let rec run_test ?(prefix = "") : test -> unit =
 fun t ->
  match t with
  | Test case -> Alcotest.run "isolated test" [ "", [ case ] ]
  | Test_suite (name, lst) ->
    if test_height t <= 3
    then (
      let name, tests = extract_param t in
      Alcotest.run (prefix ^ name) tests)
    else List.iter ~f:(run_test ~prefix:(prefix ^ name ^ "_")) lst


let wrap_ref file f =
  let s = ref None in
  fun () ->
    match !s with
    | Some (a, file') -> if String.equal file' file then a else f s
    | None -> f s


(* Common functions used in tests *)

let type_file ~raise ?(st = "auto") f options =
  ignore st;
  Build.qualified_typed ~raise ~options (Build.Source_input.From_file f)


let core_file ~raise f options =
  Build.qualified_core ~raise ~options (Build.Source_input.From_file f)


let core_file_unqualified ~raise f options = Build.unqualified_core ~raise ~options f

let get_program ~raise ?(st = "auto") f =
  wrap_ref f (fun s ->
      let program = type_file ~raise ~st f options in
      s := Some (program, f);
      program)


let get_program f ?st = get_program ?st f

let expression_to_core ~raise expression =
  let core =
    Ligo_compile.Of_unified.compile_expression
      ~raise
      ~options
      ~disable_initial_check:true
      expression
  in
  core


let pack_payload ~raise (program : Ast_typed.program) (payload : Ast_unified.expr)
    : bytes Lwt.t
  =
  let open Lwt.Let_syntax in
  let%bind code =
    let core = expression_to_core ~raise payload in
    let typed =
      let context =
        (* can't use the contract signature directly because
         it would force users to export declaration in Jsligo *)
        Ast_typed.to_signature program.pr_module
      in
      Ligo_compile.Of_core.compile_expression ~raise ~options ~context core
    in
    let aggregated =
      Ligo_compile.Of_typed.compile_expression ~raise ~options:options.middle_end typed
    in
    let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c
  in
  let payload_ty = code.expr_ty in
  let%bind (payload : _ Tezos_utils.Michelson.michelson) =
    Ligo_run.Of_michelson.evaluate_expression ~raise code.expr code.expr_ty
  in
  Ligo_run.Of_michelson.pack_payload ~raise payload payload_ty


let sign_message ~raise (program : Ast_typed.program) (payload : Ast_unified.expr) sk
    : string Lwt.t
  =
  let open Lwt.Let_syntax in
  let open Tezos_crypto in
  let%map packed_payload = pack_payload ~raise program payload in
  let signed_data = Signature.sign sk packed_payload in
  let signature_str = Signature.to_b58check signed_data in
  signature_str


let contract id =
  let open Lwt.Let_syntax in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let%map env = test_environment () in
  let id = List.nth_exn env.identities id in
  id.implicit_contract


let addr id =
  let open Proto_alpha_utils.Memory_proto_alpha in
  Lwt.map Protocol.Alpha_context.Contract.to_b58check @@ contract id


let gen_keys () =
  let open Tezos_crypto in
  let raw_pkh, raw_pk, raw_sk = Signature.generate_key () in
  raw_pkh, raw_pk, raw_sk


let str_keys (raw_pkh, raw_pk, raw_sk) =
  let open Tezos_crypto in
  let sk_str = Signature.Secret_key.to_b58check raw_sk in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  pkh_str, pk_str, sk_str


let typed_program_with_imperative_input_to_michelson
    ~raise
    (program : Ast_typed.program)
    (entry_point : string)
    (input : Ast_unified.expr)
    : (Stacking.compiled_expression * Ast_aggregated.type_expression) Lwt.t
  =
  let open Lwt.Let_syntax in
  Printexc.record_backtrace true;
  let core = expression_to_core ~raise input in
  let entry_point =
    Ligo_prim.Value_var.of_input_var ~loc:Location.generated entry_point
  in
  let app = Ligo_compile.Of_core.apply entry_point core in
  let typed_app =
    let context =
      (* can't use the contract signature directly because
       it would force users to export declaration in Jsligo *)
      Ast_typed.to_signature program.pr_module
    in
    Ligo_compile.Of_core.compile_expression ~raise ~options ~context app
  in
  let aggregated =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      None
      program
      typed_app
  in
  let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
  let%map mich = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  mich, aggregated.type_expression


let typed_program_with_imperative_input_to_michelson_twice
    ~raise
    (program : Ast_typed.program)
    (entry_point : string)
    (input1 : Ast_unified.expr)
    (input2 : Ast_unified.expr)
    : (Stacking.compiled_expression * Ast_aggregated.type_expression) Lwt.t
  =
  let open Lwt.Let_syntax in
  Printexc.record_backtrace true;
  let core1 = expression_to_core ~raise input1 in
  let core2 = expression_to_core ~raise input2 in
  let app = Ligo_compile.Of_core.apply_twice entry_point core1 core2 in
  let typed_app =
    let context =
      (* can't use the contract signature directly because
       it would force users to export declaration in Jsligo *)
      Ast_typed.to_signature program.pr_module
    in
    Ligo_compile.Of_core.compile_expression ~raise ~options ~context app
  in
  (* let compiled_applied = Ligo_compile.Of_typed.compile_expression ~raise typed_app in *)
  let aggregated =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      None
      program
      typed_app
  in
  let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
  let%map mich = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  mich, aggregated.type_expression


let run_typed_program_with_imperative_input
    ~raise
    ?options
    (program : Ast_typed.program)
    (entry_point : string)
    (input : Ast_unified.expr)
    : Ast_core.expression Lwt.t
  =
  let open Lwt.Let_syntax in
  let%bind michelson_program, ty =
    typed_program_with_imperative_input_to_michelson ~raise program entry_point input
  in
  let%map michelson_output =
    Ligo_run.Of_michelson.run_no_failwith
      ~raise
      ?options
      michelson_program.expr
      michelson_program.expr_ty
  in
  let res =
    Decompile.Of_michelson.decompile_expression
      ~raise
      ty
      (Runned_result.Success michelson_output)
  in
  match res with
  | Runned_result.Success exp -> exp
  | Runned_result.Fail _ -> raise.error test_not_expected_to_fail


let run_typed_program_with_imperative_input_twice
    ~raise
    ?options
    (program : Ast_typed.program)
    (entry_point : string)
    (input1 : Ast_unified.expr)
    (input2 : Ast_unified.expr)
    : Ast_core.expression Lwt.t
  =
  let open Lwt.Let_syntax in
  let%bind michelson_program, ty =
    typed_program_with_imperative_input_to_michelson_twice
      ~raise
      program
      entry_point
      input1
      input2
  in
  let%map michelson_output =
    Ligo_run.Of_michelson.run_no_failwith
      ~raise
      ?options
      michelson_program.expr
      michelson_program.expr_ty
  in
  let res =
    Decompile.Of_michelson.decompile_expression
      ~raise
      ty
      (Runned_result.Success michelson_output)
  in
  match res with
  | Runned_result.Success exp -> exp
  | Runned_result.Fail _ -> raise.error test_not_expected_to_fail


let expect ~raise ?options program entry_point input expecter =
  let result =
    Lwt_main.run
    @@ trace ~raise (test_run_tracer entry_point)
    @@ run_typed_program_with_imperative_input ?options program entry_point input
  in
  expecter result


let expect_twice ~raise ?options program entry_point input1 input2 expecter =
  let result =
    Lwt_main.run
    @@ trace ~raise (test_run_tracer entry_point)
    @@ run_typed_program_with_imperative_input_twice
         ?options
         program
         entry_point
         input1
         input2
  in
  expecter result


let expect_fail ~raise ?options program entry_point input =
  trace ~raise (test_run_tracer entry_point)
  @@ fun ~raise ->
  Assert.assert_fail ~raise test_expected_to_fail
  @@ fun ~raise ->
  Lwt_main.run
  @@ run_typed_program_with_imperative_input ~raise ?options program entry_point input


let expect_fail_twice ~raise ?options program entry_point input1 input2 =
  trace ~raise (test_run_tracer entry_point)
  @@ fun ~raise ->
  Assert.assert_fail ~raise test_expected_to_fail
  @@ fun ~raise ->
  Lwt_main.run
  @@ run_typed_program_with_imperative_input_twice
       ~raise
       ?options
       program
       entry_point
       input1
       input2


let expect_string_failwith ~raise ?options program entry_point input expected_failwith =
  Lwt_main.run
  @@
  let open Lwt.Let_syntax in
  let%bind michelson_program, _ =
    typed_program_with_imperative_input_to_michelson ~raise program entry_point input
  in
  let%map err =
    Ligo_run.Of_michelson.run_failwith
      ~raise
      ?options
      michelson_program.expr
      michelson_program.expr_ty
  in
  match err with
  | String (_, str) when String.equal str expected_failwith -> ()
  | _ -> raise.error test_expected_to_fail


let expect_string_failwith_twice
    ~raise
    ?options
    program
    entry_point
    input1
    input2
    expected_failwith
  =
  Lwt_main.run
  @@
  let open Lwt.Let_syntax in
  let%bind michelson_program, _ =
    typed_program_with_imperative_input_to_michelson_twice
      ~raise
      program
      entry_point
      input1
      input2
  in
  let%map err =
    Ligo_run.Of_michelson.run_failwith
      ~raise
      ?options
      michelson_program.expr
      michelson_program.expr_ty
  in
  match err with
  | String (_, str) when String.equal str expected_failwith -> ()
  | _ -> raise.error test_expected_to_fail


let expect_eq ~raise ?options program entry_point input expected =
  let expected = expression_to_core ~raise expected in
  let expecter result =
    trace_option ~raise (test_expect_tracer expected result)
    @@ Ast_core.Misc.assert_value_eq (expected, result)
  in
  expect ~raise ?options program entry_point input expecter


let expect_eq_twice ~raise ?options program entry_point input1 input2 expected =
  let expected = expression_to_core ~raise expected in
  let expecter result =
    trace_option ~raise (test_expect_tracer expected result)
    @@ Ast_core.Misc.assert_value_eq (expected, result)
  in
  expect_twice ~raise ?options program entry_point input1 input2 expecter


let expect_eq_core ~raise ?options program entry_point input expected =
  let expecter result =
    trace_option ~raise (test_expect_tracer expected result)
    @@ Ast_core.Misc.assert_value_eq (expected, result)
  in
  expect ~raise ?options program entry_point input expecter


let expect_evaluate ~raise (program : Ast_typed.program) entry_point expecter =
  let open Lwt.Let_syntax in
  trace ~raise (test_run_tracer entry_point)
  @@ fun ~raise ->
  Lwt_main.run
  @@
  let aggregated =
    Ligo_compile.Of_typed.apply_to_var
      ~raise
      ~options:options.middle_end
      program
      entry_point
  in
  let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
  let%bind michelson_value =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c
  in
  let%map res_michelson =
    Ligo_run.Of_michelson.run_no_failwith
      ~raise
      michelson_value.expr
      michelson_value.expr_ty
  in
  let res =
    Decompile.Of_michelson.decompile_expression
      ~raise
      aggregated.type_expression
      (Runned_result.Success res_michelson)
  in
  let res' =
    match res with
    | Runned_result.Success exp -> exp
    | Runned_result.Fail _ -> raise.error test_not_expected_to_fail
  in
  expecter res'


let expect_eq_evaluate ~raise (program : Ast_typed.program) entry_point expected =
  let expected = expression_to_core ~raise expected in
  let expecter result =
    trace_option ~raise (test_expect_tracer expected result)
    @@ Ast_core.Misc.assert_value_eq (expected, result)
  in
  expect_evaluate ~raise program entry_point expecter


let expect_eq_n_trace_aux_twice
    ~raise
    ?options
    lst
    program
    entry_point
    make_input
    make_expected
  =
  let aux n =
    let input1, input2 = make_input n in
    let expected = make_expected n in
    trace ~raise (test_expect_n_tracer n)
    @@ expect_eq_twice ?options program entry_point input1 input2 expected
  in
  let _ = List.map ~f:aux lst in
  ()


let expect_eq_n_aux ~raise ?options lst program entry_point make_input make_expected =
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    trace ~raise (test_expect_eq_n_tracer n)
    @@ expect_eq ?options program entry_point input expected
  in
  let () = List.iter ~f:aux lst in
  ()


let expect_eq_n_aux_twice ~raise ?options lst program entry_point make_input make_expected
  =
  let aux n =
    let input1, input2 = make_input n in
    let expected = make_expected n in
    trace ~raise (test_expect_eq_n_tracer n)
    @@ expect_eq_twice ?options program entry_point input1 input2 expected
  in
  let () = List.iter ~f:aux lst in
  ()


let expect_eq_n ?options = expect_eq_n_aux ?options [ 0; 1; 2; 42; 163; -1 ]
let expect_eq_n_twice ?options = expect_eq_n_aux_twice ?options [ 0; 1; 2; 42; 163; -1 ]
let expect_eq_n_pos ?options = expect_eq_n_aux ?options [ 0; 1; 2; 42; 163 ]
let expect_eq_n_pos_small ?options = expect_eq_n_aux ?options [ 0; 1; 2; 10 ]
let expect_eq_n_strict_pos_small ?options = expect_eq_n_aux ?options [ 1; 2; 10 ]
let expect_eq_n_pos_mid = expect_eq_n_aux [ 0; 1; 2; 10; 33 ]

let expect_eq_b ~raise program entry_point make_expected =
  let open Ast_unified in
  let aux b =
    let input = e_bool ~loc b in
    let expected = make_expected b in
    expect_eq ~raise program entry_point input expected
  in
  let () = List.iter ~f:aux [ false; true ] in
  ()


let expect_eq_n_int a b c =
  let open Ast_unified in
  expect_eq_n a b (e_int ~loc) (fun n -> e_int ~loc (c n))


let expect_eq_b_bool a b c =
  let open Ast_unified in
  expect_eq_b a b (fun bool -> e_bool ~loc (c bool))


let compile_main ~raise f () =
  let open Lwt.Let_syntax in
  let typed_prg = get_program ~raise f () in
  let typed_prg =
    Simple_utils.Trace.trace ~raise self_ast_typed_tracer
    @@ Self_ast_typed.all_program typed_prg
  in
  let _, contract_info =
    Option.value_exn ~message:"not a contract"
    @@ Ast_typed.get_contract_signature (Ast_typed.to_extended_signature typed_prg) []
  in
  let agg =
    Ligo_compile.Of_typed.apply_to_entrypoint_with_contract_type
      ~raise
      ~options:options.middle_end
      typed_prg
      []
      contract_info
  in
  let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise agg in
  let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
  Lwt_main.run
  @@ let%bind michelson_prg =
       Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c
     in
     let%map _contract : _ Tezos_utils.Michelson.michelson Lwt.t =
       (* fails if the given entry point is not a valid contract *)
       Ligo_compile.Of_michelson.build_contract
         ~raise
         michelson_prg
         []
     in
     ()
