open Simple_utils.Display
module Snippet    = Simple_utils.Snippet
module Location   = Simple_utils.Location
module PP_helpers = Simple_utils.PP_helpers

let rec error_ppformat : display_format:string display_format ->
  Format.formatter -> Types.all -> unit =
  fun ~display_format f a ->
  let is_dummy_location loc =
    Location.is_dummy_or_generated loc ||
      match Location.get_file loc with
      | Some r -> String.equal r#file ""
      | None -> true in
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Build_error_tracer err -> BuildSystem.Errors.error_ppformat ~display_format f err
    | `Test_err_tracer (name,err) ->
      Format.fprintf f "@[<hv>Test '%s'@ %a@]"
        name (error_ppformat ~display_format) err
    | `Test_run_tracer (ep, err) ->
      Format.fprintf f "@[<hv>Running entrypoint '%s'@ %a@]"
        ep (error_ppformat ~display_format) err
    | `Test_expect_tracer (expected, actual) ->
      Format.fprintf f "@[<hv>Expected:@ %a@ got:@ %a@]"
        Ast_core.PP.expression expected
        Ast_core.PP.expression actual
    | `Test_expect_n_tracer (i,err) ->
      Format.fprintf f "@[<hv>Expect n=%d@ %a@]"
        i (error_ppformat ~display_format) err
    | `Test_expect_exp_tracer (e,err) ->
      Format.fprintf f "@[<hv>Expect %a@ %a@]"
        Ast_core.PP.expression e
        (error_ppformat ~display_format) err
    | `Test_expect_eq_n_tracer (i,err) ->
      Format.fprintf f "@[<hv>Expected eq_n=%d@ %a@]"
        i (error_ppformat ~display_format) err
    | `Test_internal loc ->
      Format.fprintf f "@[<hv>Internal error:@ %s@]" loc
    | `Test_internal_msg (loc, msg) ->
      Format.fprintf f "@[<hv>Internal error:@ %s@ %s@]" loc msg
    | `Test_md_file (md_file,s,grp,prg,err) ->
      let sep = "======================" in
      Format.fprintf f "@[<v>\
                          %s@,\
                          Failed to compile code block in %s@,\
                          Syntax: %s@,\
                          Group: %s@,\
                          %s@,\
                          Program:@,%s@,\
                          %s@,\
                          Error:@,## IGNORE THE LOCATION IF ANY##@,%a@,\
                          %s@,@]"
        sep md_file s grp sep prg sep
        (error_ppformat ~display_format) err sep
    | `Test_expected_to_fail -> Format.fprintf f "test was expected to fail but did not"
    | `Test_not_expected_to_fail -> Format.fprintf f "test was not expected to fail but did"
    | `Test_repl (expected, actual) ->
       Format.fprintf f "@[<hv>Expected:@ %a@ got:@ %a@]"
        (Simple_utils.PP_helpers.list_sep_d Simple_utils.PP_helpers.string)
        expected
        (Simple_utils.PP_helpers.list_sep_d Simple_utils.PP_helpers.string)
        actual

    | `Main_invalid_generator_name generator ->
      Format.fprintf f
        "@[<hv>Invalid generator option: '%s'. @.Use 'random' or 'list'. @]"
          generator
    | `Main_invalid_syntax_name syntax ->
      Format.fprintf f
        "@[<hv>Invalid syntax option: '%s'. @.Use 'pascaligo', 'cameligo', 'reasonligo', or 'jsligo'. @]"
          syntax
    | `Main_invalid_dialect_name syntax ->
      Format.fprintf f
        "@[<hv>Invalid dialect option: '%s'. @.Use 'verbose' or 'terse'. @]"
          syntax
    | `Main_invalid_protocol_version (possible,actual) ->
      Format.fprintf f
      "@[<hv>Invalid protocol version '%s'. Available versions: %a"
        actual
        (Simple_utils.PP_helpers.list_sep_d Format.pp_print_string) possible
    | `Main_invalid_typer_switch actual ->
      Format.fprintf f
      "@[<hv>Invalid typer switch '%s'. Available: 'new' 'old'"
        actual
    | `Main_invalid_extension extension ->
      Format.fprintf f
        "@[<hv>Invalid file extension '%s'. @.Use '.ligo' for PascaLIGO, '.mligo' for CameLIGO, '.religo' for ReasonLIGO, '.jsligo' for JsLIGO, or the --syntax option.@]"
        extension

    | `Main_unparse_tracer errs ->
      let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[Error(s) occurred while translating to Michelson:@.%a@]"
      (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Main_typecheck_contract_tracer (_c,err_l) ->
      let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) err_l in
      Format.fprintf f "@[<hv>Error(s) occurred while type checking the contract:@.%a@]"
      (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Main_could_not_serialize errs ->
      let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while serializing Michelson code:@.%a @]"
      (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Check_typed_arguments_tracer (Simple_utils.Runned_result.Check_parameter, err) ->
      Format.fprintf f "@[<hv>Invalid command line argument. @.The provided parameter does not have the correct type for the given entrypoint.@ %a@]"
        (error_ppformat ~display_format) err

    | `Check_typed_arguments_tracer (Simple_utils.Runned_result.Check_storage, err) ->
      Format.fprintf f "@[<hv>Invalid command line argument. @.The provided storage does not have the correct type for the contract.@ %a@]"
        (error_ppformat ~display_format) err

    | `Main_unknown ->
      Format.fprintf f "@[<v>An unknown error occurred.@]"

    | `Main_execution_failed v ->
      Format.fprintf f
        "@[<hv>An error occurred while evaluating an expression: %a@]"
        Tezos_utils.Michelson.pp v
    | `Main_entrypoint_not_a_function -> Format.fprintf f "@[<hv>Invalid command line argument. @.The provided entrypoint is not a function.@]"
    | `Main_view_not_a_function var -> Format.fprintf f "@[<hv>Invalid command line argument. @.View \"%a\" is not a function.@]" Ligo_prim.ValueVar.pp var
    | `Main_entrypoint_not_found -> Format.fprintf f "@[<hv>Invalid command line argument. @.The provided entrypoint is not found in the contract.@]"
    | `Main_invalid_balance a -> Format.fprintf f "@[<hv>Invalid command line option \"--balance\". @.The provided balance \"%s\" is invalid. Use an integer instead. @]" a
    | `Main_invalid_amount a -> Format.fprintf f "@[<hv>Invalid command line option \"--amount\". @.The provided amount \"%s\" is invalid. Use an integer instead. @]" a
    | `Main_invalid_source a -> Format.fprintf f "@[<hv>Invalid command line option \"--source\". @.The provided source address \"%s\" is invalid. A valid Tezos address is a string prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.@]" a
    | `Main_invalid_sender a -> Format.fprintf f "@[<hv>Invalid command line option \"--sender\". @.The provided sender address \"%s\" is invalid. A valid Tezos address is a string prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.@]" a
    | `Main_invalid_timestamp t -> Format.fprintf f "@[<hv>Invalid command line option \"--now\". @.The provided now \"%s\" is invalid. It should use RFC3339 notation in a string, or the number of seconds since Epoch.@]" t
    | `Main_cannot_open_global_constants s -> Format.fprintf f "@[<hv>Cannot open global constants file. @.Check that the provided file \"%s\" exists.@]" s
    | `Main_cannot_parse_global_constants (fn, s) -> Format.fprintf f "@[<hv>Cannot parse global constants file: %s. @.Check that the provided file consists of JSON list of strings (one string per Michelson constant). @.JSON Error: %s@]" fn s

    | `Unparsing_michelson_tracer errs ->
      let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while unparsing the Michelson result:@.%a @]"
      (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Parsing_payload_tracer _ -> Format.fprintf f "@[<hv>Error parsing message. @]" (* internal testing *)
    | `Packing_payload_tracer _ -> Format.fprintf f "@[<hv>Error packing message. @]" (* internal testing *)
    | `Parsing_input_tracer errs ->
      let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while parsing the Michelson input:@.%a @]"
      (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Parsing_code_tracer errs ->
      let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while checking the contract:@.%a @]"
        (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Error_of_execution_tracer errs ->
      let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while executing the contract:@.%a @]"
      (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Preproc_tracer e -> Preprocessing.Errors.error_ppformat ~display_format f e
    | `Parser_tracer e -> Parsing.Errors.error_ppformat ~display_format f e
    | `Pretty_tracer _e -> () (*no error in this pass*)
    | `Cit_pascaligo_tracer  e -> List.iter ~f:(Tree_abstraction.Pascaligo.Errors.error_ppformat ~display_format f) e
    | `Cit_cameligo_tracer   e -> List.iter ~f:(Tree_abstraction.Cameligo.Errors.error_ppformat ~display_format f) e
    | `Cit_reasonligo_tracer e -> List.iter ~f:(Tree_abstraction.Reasonligo.Errors.error_ppformat ~display_format f) e
    | `Cit_jsligo_tracer     e -> List.iter ~f:(Tree_abstraction.Jsligo.Errors.error_ppformat ~display_format f) e
    | `Self_ast_imperative_tracer e -> Self_ast_imperative.Errors.error_ppformat ~display_format f e
    | `Purification_tracer e -> List.iter ~f:(Purification.Errors.error_ppformat ~display_format f) e
    | `Depurification_tracer _e -> () (*no error in this pass*)
    | `Desugaring_tracer _e -> () (*no error in this pass*)
    | `Sugaring_tracer _e -> () (*no error in this pass*)
    | `Checking_tracer e -> Checking.Errors.error_ppformat ~display_format f e
    | `Self_ast_typed_tracer e -> Self_ast_typed.Errors.error_ppformat ~display_format f e
    | `Aggregation_tracer e -> Aggregation.Errors.error_ppformat ~display_format f e
    | `Self_ast_aggregated_tracer e -> Self_ast_aggregated.Errors.error_ppformat ~display_format f e
    | `Self_mini_c_tracer e -> Self_mini_c.Errors.error_ppformat ~display_format f e
    | `Spilling_tracer e -> Spilling.Errors.error_ppformat ~display_format f  e
    | `Scoping_tracer e -> Scoping.Errors.error_ppformat ~display_format f e
    | `Stacking_tracer e -> Stacking.Errors.error_ppformat ~display_format f e

    | `Main_interpret_not_enough_initial_accounts (loc,max) ->
      Format.fprintf f "@[<hv>%a@. baker account initial balance must at least reach %a tez @]"
        Snippet.pp loc
        Memory_proto_alpha.Protocol.Alpha_context.Tez.pp max
    | `Main_interpret_test_entry_not_found s ->
      Format.fprintf f "Test entry '%s' not found" s
    | `Main_interpret_target_lang_error (loc, [], errs) ->
      Format.fprintf f "@[<v 4>%a@.An uncaught error occured:@.%a@]"
        Snippet.pp loc
        (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs
    | `Main_interpret_target_lang_error (loc, calltrace, errs) ->
      if not (is_dummy_location loc) || List.is_empty calltrace then
         Format.fprintf f "@[<v 4>%a@.An uncaught error occured:@.%a@.Trace:@.%a@]"
           Snippet.pp loc
           (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs
           (PP_helpers.list_sep_d Location.pp) calltrace
       else
         Format.fprintf f "@[<v 4>%a@.An uncaught error occured:@.%a@.Trace:@.%a@.%a@]"
           Snippet.pp loc
           (Tezos_client_014_PtKathma.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs
           Snippet.pp (List.hd_exn calltrace)
           (PP_helpers.list_sep_d Location.pp) (List.tl_exn calltrace)
    | `Main_interpret_target_lang_failwith (loc, v) ->
      Format.fprintf f "@[<v 4>%a@.An uncaught error occured:@.Failwith: %a@]"
        Snippet.pp loc
        Tezos_utils.Michelson.pp v
    | `Main_interpret_boostrap_not_enough loc ->
      Format.fprintf f "@[<hv>%a@.We need at least two boostrap accounts for the default source and baker@]"
      Snippet.pp loc
    | `Main_interpret_meta_lang_eval (loc,[],reason) ->
      Format.fprintf f "@[<hv>%a@.%a@]"
        Snippet.pp loc
        Ligo_interpreter.PP.pp_value reason
    | `Main_interpret_meta_lang_eval (loc,calltrace,reason) ->
      if not (is_dummy_location loc) || List.is_empty calltrace then
        Format.fprintf f "@[<hv>%a@.%a@.Trace:@.%a@]"
          Snippet.pp loc
          Ligo_interpreter.PP.pp_value reason
          (PP_helpers.list_sep_d Location.pp) calltrace
      else
        Format.fprintf f "@[<hv>%a@.%a@.Trace:@.%a@.%a@]"
          Snippet.pp loc
          Ligo_interpreter.PP.pp_value reason
          Snippet.pp (List.hd_exn calltrace)
          (PP_helpers.list_sep_d Location.pp) (List.tl_exn calltrace)
    | `Main_interpret_meta_lang_failwith (loc,[],value) ->
      Format.fprintf f "@[<hv>%a@.Test failed with %a@]"
        Snippet.pp loc
        Ligo_interpreter.PP.pp_value value
    | `Main_interpret_meta_lang_failwith (loc,calltrace,value) ->
      if not (is_dummy_location loc) || List.is_empty calltrace then
        Format.fprintf f "@[<hv>%a@.Test failed with %a@.Trace:@.%a@]"
          Snippet.pp loc
          Ligo_interpreter.PP.pp_value value
          (PP_helpers.list_sep_d Location.pp) calltrace
      else
        Format.fprintf f "@[<hv>%a@.Test failed with %a@.Trace:@.%a@.%a@]"
          Snippet.pp loc
          Ligo_interpreter.PP.pp_value value
           Snippet.pp (List.hd_exn calltrace)
           (PP_helpers.list_sep_d Location.pp) (List.tl_exn calltrace)
    | `Main_interpret_generic (loc,desc) ->
      Format.fprintf f "@[<hv>%a@.%s@]"
        Snippet.pp loc
        desc
    | `Main_interpret_literal (loc,l) ->
      Format.fprintf f "@[<hv>%a@.Invalid interpretation of literal: %a@]"
        Snippet.pp loc
        Ligo_prim.Literal_value.pp l
    | `Main_interpret_modules_not_supported loc ->
      Format.fprintf f "@[<hv>%a@.Module are not handled in interpreter yet@]"
      Snippet.pp loc

    | `Main_decompile_michelson e -> Stacking.Errors.error_ppformat ~display_format f  e
    | `Main_decompile_mini_c e -> Spilling.Errors.error_ppformat ~display_format f  e
    | `Main_decompile_aggregated e -> Aggregation.Errors.error_ppformat ~display_format f  e
    | `Main_decompile_typed e -> Checking.Errors.error_ppformat ~display_format f  e
    | `Main_view_rule_violated loc ->
      Format.fprintf f "@[<hv>%a@.View rule violated:
      - Tezos.create_contract ; Tezos.set_delegate and Tezos.transaction cannot be used because they are stateful (expect in lambdas)
      - Tezos.self can't be used because the entry-point does not make sense in a view@.@]"
      Snippet.pp loc
    | `Repl_unexpected -> Format.fprintf f "unexpected error, missing expression?"
    | `Ligo_init_unrecognized_template lststr -> Format.fprintf f "Template unrecognized please select one of the following list : \n%s" @@ String.concat ~sep:"\n" lststr
  )

let json_error ~stage ?message ?child ?(loc=Location.generated) ?(extra_content=[]) () =
  let append_opt ~f xs opt = Option.fold opt ~init:xs ~f:(fun xs x -> f x :: xs) in
  let content = extra_content in
  let content = append_opt content message  ~f:(fun msg -> ("message", `String msg)) in
  let content = append_opt content child ~f:(fun err -> ("children", err))
  in
  `Assoc [
    ("status",  `String "error") ;
    ("stage",   `String stage)   ;
    ("content", `Assoc content) ;
    ("location", Location.to_yojson loc)]


let rec error_jsonformat : Types.all -> Yojson.Safe.t = fun a ->
  match a with
  | `Test_err_tracer (name,err) ->
    json_error ~stage:"test_tracer" ~message:name ~child:(error_jsonformat err) ()

  | `Test_run_tracer _
  | `Test_expect_tracer _
  | `Test_expect_n_tracer _
  | `Test_expect_exp_tracer _
  | `Test_expect_eq_n_tracer _
  | `Test_internal _
  | `Test_internal_msg _
  | `Test_md_file _
  | `Test_expected_to_fail
  | `Test_not_expected_to_fail
  | `Test_repl _
  -> `Null

  (* Top-level errors *)
  | `Build_error_tracer e -> json_error ~stage:"build system" ~child:(BuildSystem.Errors.error_jsonformat e) ()
  | `Main_invalid_generator_name _ ->
    json_error ~stage:"command line interpreter" ~message:"bad generator name" ()
  | `Main_invalid_syntax_name _ ->
    json_error ~stage:"command line interpreter" ~message:"bad syntax name" ()
  | `Main_invalid_dialect_name _ ->
    json_error ~stage:"command line interpreter" ~message:"bad dialect name" ()
  | `Main_invalid_protocol_version _ ->
    json_error ~stage:"command line interpreter" ~message:"bad protocol version" ()
  | `Main_invalid_typer_switch _ ->
    json_error ~stage:"command line interpreter" ~message:"bad typer switch" ()
  | `Main_invalid_extension _ ->
    json_error ~stage:"command line interpreter" ~message:"bad file extension" ()

  | `Main_unparse_tracer _ ->
    json_error ~stage:"michelson contract build" ~message:"could not unparse michelson type" ()

  | `Main_typecheck_contract_tracer (c,_) ->
    let code = Format.asprintf "%a" Tezos_utils.Michelson.pp c in
    let extra_content = [("code", `String code)] in
    json_error ~stage:"michelson contract build" ~message:"Could not typecheck michelson code" ~extra_content ()

  | `Main_could_not_serialize _errs ->
    json_error ~stage:"michelson serialization" ~message:"Could not serialize michelson code" ()

  | `Check_typed_arguments_tracer (Simple_utils.Runned_result.Check_parameter, err) ->
    json_error ~stage:"contract argument typechecking"
        ~message:"Passed parameter does not match the contract type"
        ~child:(error_jsonformat err) ()

  | `Check_typed_arguments_tracer (Simple_utils.Runned_result.Check_storage, err) ->
    json_error ~stage:"contract argument typechecking"
        ~message:"Passed storage does not match the contract type"
        ~child:(error_jsonformat err) ()

  | `Main_unknown ->
    json_error ~stage:"michelson execution" ~message:"unknown error" ()

  | `Main_execution_failed v ->
    let value =
      Format.asprintf "%a"
      Tezos_utils.Michelson.pp v
    in
    let extra_content = [("failwith", `String value)] in
    json_error ~stage:"michelson execution" ~extra_content ()

  | `Main_invalid_amount a ->
    let message = "invalid amount" in
    let extra_content = [("value", `String a)] in
    json_error ~stage:"parsing command line parameters" ~message ~extra_content ()
  | `Main_invalid_balance a ->
    let message = "invalid balance" in
    let extra_content = [("value", `String a)] in
    json_error ~stage:"parsing command line parameters" ~message ~extra_content ()
  | `Main_invalid_source a ->
    let message = "invalid source" in
    let extra_content = [("value", `String a)] in
    json_error ~stage:"parsing command line parameters" ~message ~extra_content ()
  | `Main_invalid_sender a ->
    let message = "invalid sender" in
    let extra_content = [("value", `String a)] in
    json_error ~stage:"parsing command line parameters" ~message ~extra_content ()
  | `Main_invalid_timestamp t ->
    let message = "invalid timestamp notation" in
    let extra_content = [("value", `String t)] in
    json_error ~stage:"parsing command line parameters" ~message ~extra_content ()
  | `Main_cannot_open_global_constants _ ->
    json_error ~stage:"global constants parsing" ~message:"cannot open global constants file" ()
  | `Main_cannot_parse_global_constants _ ->
    json_error ~stage:"global constants parsing" ~message:"cannot parse global constants file" ()

  | `Unparsing_michelson_tracer _ ->
    json_error ~stage:"michelson execution" ~message:"error unparsing michelson result" ()

  | `Parsing_payload_tracer _ ->
    json_error ~stage:"michelson execution" ~message:"error parsing message" ()

  | `Packing_payload_tracer _ ->
    json_error ~stage:"michelson execution" ~message:"error packing message" ()

  | `Parsing_input_tracer _ ->
    json_error ~stage:"michelson execution" ~message:"error parsing input" ()

  | `Parsing_code_tracer _ ->
    json_error ~stage:"michelson execution" ~message:"error parsing program code" ()

  | `Error_of_execution_tracer _ ->
    json_error ~stage:"michelson execution" ~message:"error of execution" ()

  | `Main_entrypoint_not_a_function -> json_error ~stage:"top-level glue" ~message:"given entrypoint is not a function" ()
  | `Main_view_not_a_function _str -> json_error ~stage:"top-level glue" ~message:"given view is not a function" ()
  | `Main_view_rule_violated loc -> json_error ~loc ~stage:"top-level glue" ~message:"view rule violated" ()
  | `Main_entrypoint_not_found -> json_error ~stage:"top-level glue" ~message:"Missing entrypoint" ()

  | `Preproc_tracer e -> Preprocessing.Errors.error_jsonformat e
  | `Parser_tracer e -> Parsing.Errors.error_jsonformat e
  | `Pretty_tracer _ -> `Null (*no error in this pass*)
  | `Cit_pascaligo_tracer  e -> `List (List.map ~f:Tree_abstraction.Pascaligo.Errors.error_jsonformat e)
  | `Cit_cameligo_tracer   e -> `List (List.map ~f:Tree_abstraction.Cameligo.Errors.error_jsonformat e)
  | `Cit_reasonligo_tracer e -> `List (List.map ~f:Tree_abstraction.Reasonligo.Errors.error_jsonformat e)
  | `Cit_jsligo_tracer     e -> `List (List.map ~f:Tree_abstraction.Jsligo.Errors.error_jsonformat e)
  | `Self_ast_imperative_tracer e -> Self_ast_imperative.Errors.error_jsonformat e
  | `Purification_tracer e -> `List (List.map ~f:Purification.Errors.error_jsonformat e)
  | `Depurification_tracer _ -> `Null (*no error in this pass*)
  | `Desugaring_tracer _ -> `Null (*no error in this pass*)
  | `Sugaring_tracer _ -> `Null (*no error in this pass*)
  | `Checking_tracer e -> Checking.Errors.error_jsonformat e
  | `Self_ast_typed_tracer e -> Self_ast_typed.Errors.error_jsonformat e
  | `Aggregation_tracer e -> Aggregation.Errors.error_jsonformat e
  | `Self_ast_aggregated_tracer e -> Self_ast_aggregated.Errors.error_jsonformat e
  | `Spilling_tracer e -> Spilling.Errors.error_jsonformat e
  | `Self_mini_c_tracer e -> Self_mini_c.Errors.error_jsonformat e
  | `Scoping_tracer e -> Scoping.Errors.error_jsonformat e
  | `Stacking_tracer e -> Stacking.Errors.error_jsonformat e

  | `Main_interpret_test_entry_not_found _
  | `Main_interpret_target_lang_error _
  | `Main_interpret_target_lang_failwith _
  | `Main_interpret_boostrap_not_enough _
  | `Main_interpret_meta_lang_eval _
  | `Main_interpret_meta_lang_failwith _
  | `Main_interpret_generic _
  | `Main_interpret_literal _
  | `Main_interpret_modules_not_supported _
  | `Main_interpret_not_enough_initial_accounts _
   -> `Null

  | `Main_decompile_michelson e -> Stacking.Errors.error_jsonformat e
  | `Main_decompile_mini_c e -> Spilling.Errors.error_jsonformat e
  | `Main_decompile_aggregated e -> Aggregation.Errors.error_jsonformat e
  | `Main_decompile_typed e -> Checking.Errors.error_jsonformat e
  | `Ligo_init_unrecognized_template _lsttr -> `Null
  | `Repl_unexpected ->
     let message = "unexpected error" in
     json_error ~stage:"evaluating expression" ~message ()

let error_format : _ Simple_utils.Display.format = {
  pp = error_ppformat;
  to_json = error_jsonformat;
}
