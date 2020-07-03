open Display

let error_suggest: string = "\n
If you're not sure how to fix this error, you can do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/introduction
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'"

let rec error_ppformat' : display_format:string display_format ->
  Format.formatter -> Types.all -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with 
    | `Test_err_tracer (name,err) ->
      Format.fprintf f "@[<hv>Test '%s'@ %a@]"
        name (error_ppformat' ~display_format) err
    | `Test_run_tracer (ep, err) ->
      Format.fprintf f "@[<hv>Running entrypoint '%s'@ %a@]"
        ep (error_ppformat' ~display_format) err
    | `Test_expect_tracer (expected, actual) ->
      Format.fprintf f "@[<hv>Expected:@ %a@ got:@ %a@]"
        Ast_core.PP.expression expected
        Ast_core.PP.expression actual
    | `Test_expect_n_tracer (i,err) ->
      Format.fprintf f "@[<hv>Expect n=%d@ %a@]"
        i (error_ppformat' ~display_format) err
    | `Test_expect_exp_tracer (e,err) ->
      Format.fprintf f "@[<hv>Expect %a@ %a@]"
        Ast_core.PP.expression e
        (error_ppformat' ~display_format) err
    | `Test_expect_eq_n_tracer (i,err) ->
      Format.fprintf f "@[<hv>Expected eq_n=%d@ %a@]"
        i (error_ppformat' ~display_format) err
    | `Test_internal t ->
      Format.fprintf f "@[<hv>Internal error:@ %s@]" t
    | `Test_md_file_tracer (md_file,s,grp,prg,err) ->
      Format.fprintf f "@[<hv>Failed to compile %s@ syntax: %s@ group: %s@ program: %s@ %a@]"
      md_file s grp prg (error_ppformat' ~display_format) err
    | `Test_bad_code_block arg ->
      Format.fprintf f "@[<hv>Bad code block argument '%s'@ only 'group=NAME' or 'skip' are allowed@]"
        arg
    | `Test_expected_to_fail -> Format.fprintf f "test was expected to fail but did not"
    | `Test_not_expected_to_fail -> Format.fprintf f "test was not expected to fail but did"

    | `Main_invalid_syntax_name syntax ->
      Format.fprintf f
        "@[<hv>Invalid syntax name '%s'@ Hint: Use 'pascaligo', 'cameligo' or 'reasonligo'@]"
          syntax

    | `Main_invalid_extension extension ->
      Format.fprintf f
        "@[<hv>Invalid extension '%s'@ Hint: Use '.ligo', '.mligo', '.religo' or the --syntax option@]"
        extension

    | `Main_bad_michelson_parameter c ->
      let s = Format.asprintf
        "generated Michelson contract failed to typecheck : bad contract parameter type\n\
        code:\n %a" Michelson.pp c in
      Format.pp_print_string f s

    | `Main_bad_michelson_storage c ->
      let s = Format.asprintf
        "generated Michelson contract failed to typecheck : bad contract storage type\n\
        code:\n %a" Michelson.pp c in
      Format.pp_print_string f s

    | `Main_bad_michelson c ->
      let s = Format.asprintf
        "generated Michelson contract failed to typecheck : bad contract type\n\
        code:\n %a" Michelson.pp c in
      Format.pp_print_string f s

    | `Main_gas_exhaustion -> Format.pp_print_string f "gas exhaustion"

    | `Main_unparse_tracer _ -> Format.pp_print_string f "could not unparse michelson type"

    | `Main_typecheck_contract_tracer (c,_) ->
      let s = Format.asprintf
        "Could not typecheck michelson code:\n %a"
        Michelson.pp c in
      Format.pp_print_string f s
    
    | `Main_typecheck_parameter -> Format.pp_print_string f "Passed parameter does not match the contract type"

    | `Main_check_typed_arguments (Simple_utils.Runned_result.Check_parameter, err) ->
      Format.fprintf f "@[<v>Provided parameter type does not match contract parameter type@ %a@]"
        (error_ppformat' ~display_format) err

    | `Main_check_typed_arguments (Simple_utils.Runned_result.Check_storage, err) ->
      Format.fprintf f "@[<v>Provided storage type does not match contract storage type@ %a@]"
        (error_ppformat' ~display_format) err

    | `Main_unknown_failwith_type ->
      Format.fprintf f "@[<v>Execution failed with an unknown failwith type@]"
    | `Main_unknown ->
      Format.fprintf f "@[<v>Unknown error@]"

    | `Main_execution_failed (fw:Runned_result.failwith) ->
      let value = match fw with
        | Failwith_int i -> string_of_int i
        | Failwith_string s -> s
        | Failwith_bytes b -> Bytes.to_string b in
      Format.fprintf f
        "[<hv>Execution failed with %s@]"
        value
    | `Main_entrypoint_not_a_function -> Format.fprintf f "@[<hv>Given entrypoint is not a function@]"
    | `Main_entrypoint_not_found -> Format.fprintf f "@[<hv>Missing entrypoint@]"
    | `Main_invalid_amount a -> Format.fprintf f "@[<hv>Invalid amount %s@]" a
    | `Main_invalid_address a -> Format.fprintf f "@[<hv>Invalid address %s@]" a
    | `Main_invalid_timestamp t -> Format.fprintf f "@[<hv>Invalid timestamp notation %s@]" t

    | `Main_unparse_michelson_result _ -> Format.fprintf f "@[<hv>Error unparsing michelson result@]"
    | `Main_parse_payload _ -> Format.fprintf f "@[<hv>Error parsing message@]"
    | `Main_pack_payload _ -> Format.fprintf f "@[<hv>Error packing message@]"
    | `Main_parse_michelson_input _ -> Format.fprintf f "@[<hv>Error parsing input@]"
    | `Main_parse_michelson_code _ -> Format.fprintf f "@[<hv>Error parsing program code@]"
    | `Main_michelson_execution_error _ -> Format.fprintf f "@[<hv>Error of execution@]"

    | `Main_parser e -> Parser.Errors.error_ppformat ~display_format f e
    | `Main_pretty _e -> () (*no error in this pass*)
    | `Main_self_ast_imperative e -> Self_ast_imperative.Errors.error_ppformat ~display_format f e 
    | `Main_purification e -> Purification.Errors.error_ppformat ~display_format f e
    | `Main_depurification _e -> () (*no error in this pass*)
    | `Main_desugaring _e -> () (*no error in this pass*)
    | `Main_sugaring _e -> () (*no error in this pass*)
    | `Main_cit_pascaligo e -> Tree_abstraction.Pascaligo.Errors.error_ppformat ~display_format f e
    | `Main_cit_cameligo e -> Tree_abstraction.Cameligo.Errors.error_ppformat ~display_format f e
    | `Main_typer e -> Typer.Errors.error_ppformat ~display_format f e
    | `Main_interpreter _ -> () (*no error*)
    | `Main_self_ast_typed e -> Self_ast_typed.Errors.error_ppformat ~display_format f e
    | `Main_self_mini_c e -> Self_mini_c.Errors.error_ppformat ~display_format f e
    | `Main_spilling e -> Spilling.Errors.error_ppformat ~display_format f  e
    | `Main_stacking e -> Stacking.Errors.error_ppformat ~display_format f e

    | `Main_decompile_michelson e -> Stacking.Errors.error_ppformat ~display_format f  e
    | `Main_decompile_mini_c e -> Spilling.Errors.error_ppformat ~display_format f  e
    | `Main_decompile_typed e -> Typer.Errors.error_ppformat ~display_format f  e
  )
  
let error_ppformat : display_format:string display_format ->
  Format.formatter -> Types.all -> unit = fun ~display_format f a ->
    Format.fprintf f "@[<v>%a@ %s@]"
      (error_ppformat' ~display_format) a
      error_suggest

let rec error_jsonformat : Types.all -> Yojson.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Test_err_tracer _
  | `Test_run_tracer _
  | `Test_expect_tracer _
  | `Test_expect_n_tracer _
  | `Test_expect_exp_tracer _
  | `Test_expect_eq_n_tracer _
  | `Test_internal _
  | `Test_md_file_tracer _
  | `Test_bad_code_block _
  | `Test_expected_to_fail
  | `Test_not_expected_to_fail
  -> `Null

  (* Top-level errors *)
  | `Main_invalid_syntax_name _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad syntax name")

  | `Main_invalid_extension _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad file extension")

  | `Main_bad_michelson_parameter c ->
    let code = Format.asprintf "%a" Michelson.pp c in
    let content = `Assoc [("message", `String "bad contract parameter type") ; ("code", `String code)] in
    json_error ~stage:"michelson contract build" ~content

  | `Main_bad_michelson_storage c ->
    let code = Format.asprintf "%a" Michelson.pp c in
    let content = `Assoc [("message", `String "bad contract storage type") ; ("code", `String code)] in
    json_error ~stage:"michelson contract build" ~content

  | `Main_bad_michelson c ->
    let code = Format.asprintf "%a" Michelson.pp c in
    let content = `Assoc [("message", `String "bad contract type") ; ("code", `String code)] in
    json_error ~stage:"michelson contract build" ~content

  | `Main_gas_exhaustion ->
    let content = `Assoc [("message", `String "gas exhaustion")] in
    json_error ~stage:"michelson contract build" ~content

  | `Main_unparse_tracer _ ->
    let content = `Assoc [("message", `String "could not unparse michelson type")] in
    json_error ~stage:"michelson contract build" ~content
  
  | `Main_typecheck_contract_tracer (c,_) -> 
    let code = Format.asprintf "%a" Michelson.pp c in
    let content = `Assoc [
      ("message", `String "Could not typecheck michelson code") ;
      ("code",    `String code) ; ] in
    json_error ~stage:"michelson contract build" ~content

  | `Main_typecheck_parameter ->
    let content = `Assoc [("message", `String "Passed parameter does not match the contract type")] in
    json_error ~stage:"michelson contract build" ~content
  
  | `Main_check_typed_arguments (Simple_utils.Runned_result.Check_parameter, err) ->
    let content = `Assoc [
      ("message", `String "Passed parameter does not match the contract type");
      ("children", error_jsonformat err);
      ] in
    json_error ~stage:"contract argument typechecking" ~content

  | `Main_check_typed_arguments (Simple_utils.Runned_result.Check_storage, err) ->
    let content = `Assoc [
      ("message", `String "Passed storage does not match the contract type");
      ("children", error_jsonformat err);
      ] in
    json_error ~stage:"contract argument typechecking" ~content

  | `Main_unknown_failwith_type ->
    json_error ~stage:"michelson execution" ~content:(`String "unknown failwith type")
  | `Main_unknown ->
    json_error ~stage:"michelson execution" ~content:(`String "unknown error")

  | `Main_execution_failed (fw:Runned_result.failwith) ->
    let value = match fw with
      | Failwith_int i -> `Assoc [("value", `Int i) ; ("type", `String "int")]
      | Failwith_string s -> `Assoc [("value", `String s) ; ("type", `String "int")]
      | Failwith_bytes b -> `Assoc [("value", `String (Bytes.to_string b)) ; ("type", `String "bytes")]
    in
    let content = `Assoc [("failwith", value)] in
    json_error ~stage:"michelson execution" ~content

  | `Main_invalid_amount a ->
    let message = `String "invalid amount" in
    let value = `String a in
    let content = `Assoc [("message", message) ; ("value", value)] in
    json_error ~stage:"parsing command line parameters" ~content
  | `Main_invalid_address a ->
    let message = `String "invalid address" in
    let value = `String a in
    let content = `Assoc [("message", message) ; ("value", value)] in
    json_error ~stage:"parsing command line parameters" ~content
  | `Main_invalid_timestamp t ->
    let message = `String "invalid timestamp notation" in
    let value = `String t in
    let content = `Assoc [("message", message) ; ("value", value)] in
    json_error ~stage:"parsing command line parameters" ~content

  | `Main_unparse_michelson_result _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error unparsing michelson result")

  | `Main_parse_payload _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error parsing message")

  | `Main_pack_payload _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error packing message")

  | `Main_parse_michelson_input _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error parsing input")

  | `Main_parse_michelson_code _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error parsing program code")

  | `Main_michelson_execution_error _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error of execution")

  | `Main_entrypoint_not_a_function -> json_error ~stage:"top-level glue" ~content:(`String "given entrypoint is not a function")
  | `Main_entrypoint_not_found -> json_error ~stage:"top-level glue" ~content:(`String "Missing entrypoint")

  | `Main_parser e -> Parser.Errors.error_jsonformat e
  | `Main_pretty _ -> `Null (*no error in this pass*)
  | `Main_self_ast_imperative e -> Self_ast_imperative.Errors.error_jsonformat e
  | `Main_purification e -> Purification.Errors.error_jsonformat e
  | `Main_depurification _ -> `Null (*no error in this pass*)
  | `Main_desugaring _ -> `Null (*no error in this pass*)
  | `Main_sugaring _ -> `Null (*no error in this pass*)
  | `Main_cit_pascaligo e -> Tree_abstraction.Pascaligo.Errors.error_jsonformat e
  | `Main_cit_cameligo e -> Tree_abstraction.Cameligo.Errors.error_jsonformat e
  | `Main_typer e -> Typer.Errors.error_jsonformat e
  | `Main_interpreter _ -> `Null (*no error*)
  | `Main_self_ast_typed e -> Self_ast_typed.Errors.error_jsonformat e
  | `Main_spilling e -> Spilling.Errors.error_jsonformat e
  | `Main_self_mini_c e -> Self_mini_c.Errors.error_jsonformat e
  | `Main_stacking e -> Stacking.Errors.error_jsonformat e
  
  | `Main_decompile_michelson e -> Stacking.Errors.error_jsonformat e
  | `Main_decompile_mini_c e -> Spilling.Errors.error_jsonformat e
  | `Main_decompile_typed e -> Typer.Errors.error_jsonformat e

let error_format : _ Display.format = {
  pp = error_ppformat;
  to_json = error_jsonformat;
}
