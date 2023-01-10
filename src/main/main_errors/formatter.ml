open Simple_utils.Display
module Snippet = Simple_utils.Snippet
module Location = Simple_utils.Location
module PP_helpers = Simple_utils.PP_helpers

let rec error_ppformat
    :  display_format:string display_format -> no_colour:bool -> Format.formatter
    -> Types.all -> unit
  =
 fun ~display_format ~no_colour f a ->
  let self = error_ppformat ~display_format ~no_colour in
  let snippet_pp = Snippet.pp ~no_colour in
  let pp_value = Ligo_interpreter.PP.pp_value ~no_colour in
  let is_dummy_location loc =
    Location.is_dummy_or_generated loc
    ||
    match Location.get_file loc with
    | Some r -> String.equal r#file ""
    | None -> true
  in
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Build_error_tracer err ->
      BuildSystem.Errors.error_ppformat ~display_format ~no_colour f err
    | `Test_err_tracer (name, err) ->
      Format.fprintf f "@[<hv>Test '%s'@ %a@]" name self err
    | `Test_run_tracer (ep, err) ->
      Format.fprintf f "@[<hv>Running entrypoint '%s'@ %a@]" ep self err
    | `Test_expect_tracer (expected, actual) ->
      Format.fprintf
        f
        "@[<hv>Expected:@ %a@ got:@ %a@]"
        Ast_core.PP.expression
        expected
        Ast_core.PP.expression
        actual
    | `Test_expect_n_tracer (i, err) ->
      Format.fprintf f "@[<hv>Expect n=%d@ %a@]" i self err
    | `Test_expect_exp_tracer (e, err) ->
      Format.fprintf f "@[<hv>Expect %a@ %a@]" Ast_core.PP.expression e self err
    | `Test_expect_eq_n_tracer (i, err) ->
      Format.fprintf f "@[<hv>Expected eq_n=%d@ %a@]" i self err
    | `Test_internal loc -> Format.fprintf f "@[<hv>Internal error:@ %s@]" loc
    | `Test_internal_msg (loc, msg) ->
      Format.fprintf f "@[<hv>Internal error:@ %s@ %s@]" loc msg
    | `Test_md_file (md_file, s, grp, prg, err) ->
      let sep = "======================" in
      Format.fprintf
        f
        "@[<v>%s@,\
         Failed to compile code block in %s@,\
         Syntax: %s@,\
         Group: %s@,\
         %s@,\
         Program:@,\
         %s@,\
         %s@,\
         Error:@,\
         ## IGNORE THE LOCATION IF ANY##@,\
         %a@,\
         %s@,\
         @]"
        sep
        md_file
        s
        grp
        sep
        prg
        sep
        self
        err
        sep
    | `Test_expected_to_fail -> Format.fprintf f "test was expected to fail but did not"
    | `Test_not_expected_to_fail ->
      Format.fprintf f "test was not expected to fail but did"
    | `Test_repl (expected, actual) ->
      Format.fprintf
        f
        "@[<hv>Expected:@ %a@ got:@ %a@]"
        (Simple_utils.PP_helpers.list_sep_d Simple_utils.PP_helpers.string)
        expected
        (Simple_utils.PP_helpers.list_sep_d Simple_utils.PP_helpers.string)
        actual
    | `Main_invalid_generator_name generator ->
      Format.fprintf
        f
        "@[<hv>Invalid generator option: '%s'. @.Use 'random' or 'list'. @]"
        generator
    | `Main_invalid_syntax_name syntax ->
      Format.fprintf
        f
        "@[<hv>Invalid syntax option: '%s'. @.Use 'pascaligo', 'cameligo', 'reasonligo', \
         or 'jsligo'. @]"
        syntax
    | `Main_invalid_dialect_name syntax ->
      Format.fprintf
        f
        "@[<hv>Invalid dialect option: '%s'. @.Use 'verbose' or 'terse'. @]"
        syntax
    | `Main_invalid_protocol_version (possible, actual) ->
      Format.fprintf
        f
        "@[<hv>Invalid protocol version '%s'. Available versions: %a"
        actual
        (Simple_utils.PP_helpers.list_sep_d Format.pp_print_string)
        possible
    | `Main_invalid_extension extension ->
      Format.fprintf
        f
        "@[<hv>Invalid file extension '%s'. @.Use '.ligo' for PascaLIGO, '.mligo' for \
         CameLIGO, '.religo' for ReasonLIGO, '.jsligo' for JsLIGO, or the --syntax \
         option.@]"
        extension
    | `Main_unparse_tracer errs ->
      let errs =
        List.map
          ~f:(fun e ->
            match e with
            | `Tezos_alpha_error a -> a)
          errs
      in
      Format.fprintf
        f
        "@[Error(s) occurred while translating to Michelson:@.%a@]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
    | `Main_typecheck_contract_tracer (protocol, _c, err_l)
      when Environment.Protocols.(equal protocol in_use) ->
      let errs =
        List.map
          ~f:(fun e ->
            match e with
            | `Tezos_alpha_error a -> a)
          err_l
      in
      Format.fprintf
        f
        "@[<hv>Error(s) occurred while type checking the contract:@.%a@]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
    | `Main_typecheck_contract_tracer (_protocol, _c, err_l) ->
      let errs =
        List.map
          ~f:(fun e ->
            match e with
            | `Tezos_alpha_error a -> a)
          err_l
      in
      Format.fprintf
        f
        "@[<hv>Error(s) occurred while type checking the contract:@.%a@]"
        (Memory_proto_pre_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
    | `Main_could_not_serialize errs ->
      let errs =
        List.map
          ~f:(fun e ->
            match e with
            | `Tezos_alpha_error a -> a)
          errs
      in
      Format.fprintf
        f
        "@[<hv>Error(s) occurred while serializing Michelson code:@.%a @]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
    | `Check_typed_arguments_tracer (Simple_utils.Runned_result.Check_parameter, err) ->
      Format.fprintf
        f
        "@[<hv>Invalid command line argument. @.The provided parameter does not have the \
         correct type for the given entrypoint.@ %a@]"
        self
        err
    | `Check_typed_arguments_tracer (Simple_utils.Runned_result.Check_storage, err) ->
      Format.fprintf
        f
        "@[<hv>Invalid command line argument. @.The provided storage does not have the \
         correct type for the contract.@ %a@]"
        self
        err
    | `Main_unknown -> Format.fprintf f "@[<v>An unknown error occurred.@]"
    | `Main_execution_failed v ->
      Format.fprintf
        f
        "@[<hv>An error occurred while evaluating an expression: %a@]"
        Tezos_utils.Michelson.pp
        v
    | `Main_entrypoint_not_a_function ->
      Format.fprintf
        f
        "@[<hv>Invalid command line argument. @.The provided entrypoint is not a \
         function.@]"
    | `Main_view_not_a_function var ->
      Format.fprintf
        f
        "@[<hv>Invalid command line argument. @.View \"%a\" is not a function.@]"
        Ligo_prim.Value_var.pp
        var
    | `Main_entrypoint_not_found ->
      Format.fprintf
        f
        "@[<hv>Invalid command line argument. @.The provided entrypoint is not found in \
         the contract.@]"
    | `Main_invalid_balance a ->
      Format.fprintf
        f
        "@[<hv>Invalid command line option \"--balance\". @.The provided balance \"%s\" \
         is invalid. Use an integer instead. @]"
        a
    | `Main_invalid_amount a ->
      Format.fprintf
        f
        "@[<hv>Invalid command line option \"--amount\". @.The provided amount \"%s\" is \
         invalid. Use an integer instead. @]"
        a
    | `Main_invalid_source a ->
      Format.fprintf
        f
        "@[<hv>Invalid command line option \"--source\". @.The provided source address \
         \"%s\" is invalid. A valid Tezos address is a string prefixed by either tz1, \
         tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a \
         4-byte checksum.@]"
        a
    | `Main_invalid_sender a ->
      Format.fprintf
        f
        "@[<hv>Invalid command line option \"--sender\". @.The provided sender address \
         \"%s\" is invalid. A valid Tezos address is a string prefixed by either tz1, \
         tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a \
         4-byte checksum.@]"
        a
    | `Main_invalid_timestamp t ->
      Format.fprintf
        f
        "@[<hv>Invalid command line option \"--now\". @.The provided now \"%s\" is \
         invalid. It should use RFC3339 notation in a string, or the number of seconds \
         since Epoch.@]"
        t
    | `Main_cannot_open_global_constants s ->
      Format.fprintf
        f
        "@[<hv>Cannot open global constants file. @.Check that the provided file \"%s\" \
         exists.@]"
        s
    | `Main_cannot_parse_global_constants (fn, s) ->
      Format.fprintf
        f
        "@[<hv>Cannot parse global constants file: %s. @.Check that the provided file \
         consists of JSON list of strings (one string per Michelson constant). @.JSON \
         Error: %s@]"
        fn
        s
    | `Unparsing_michelson_tracer errs ->
      let errs =
        List.map
          ~f:(fun e ->
            match e with
            | `Tezos_alpha_error a -> a)
          errs
      in
      Format.fprintf
        f
        "@[<hv>Error(s) occurred while unparsing the Michelson result:@.%a @]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
    | `Parsing_payload_tracer _ ->
      Format.fprintf f "@[<hv>Error parsing message. @]" (* internal testing *)
    | `Packing_payload_tracer _ ->
      Format.fprintf f "@[<hv>Error packing message. @]" (* internal testing *)
    | `Parsing_input_tracer errs ->
      let errs =
        List.map
          ~f:(fun e ->
            match e with
            | `Tezos_alpha_error a -> a)
          errs
      in
      Format.fprintf
        f
        "@[<hv>Error(s) occurred while parsing the Michelson input:@.%a @]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
    | `Parsing_code_tracer errs ->
      let errs =
        List.map
          ~f:(fun e ->
            match e with
            | `Tezos_alpha_error a -> a)
          errs
      in
      Format.fprintf
        f
        "@[<hv>Error(s) occurred while checking the contract:@.%a @]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
    | `Error_of_execution_tracer errs ->
      let errs =
        List.map
          ~f:(fun e ->
            match e with
            | `Tezos_alpha_error a -> a)
          errs
      in
      Format.fprintf
        f
        "@[<hv>Error(s) occurred while executing the contract:@.%a @]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
    | `Preproc_tracer e ->
      Preprocessing.Errors.error_ppformat ~display_format ~no_colour f e
    | `Parser_tracer e -> Parsing.Errors.error_ppformat ~no_colour ~display_format f e
    | `Pretty_tracer _e -> () (*no error in this pass*)
    | `Cit_pascaligo_tracer e ->
      List.iter
        ~f:(Tree_abstraction.Pascaligo.Errors.error_ppformat ~display_format ~no_colour f)
        e
    | `Cit_cameligo_tracer e ->
      List.iter
        ~f:(Tree_abstraction.Cameligo.Errors.error_ppformat ~display_format ~no_colour f)
        e
    | `Cit_reasonligo_tracer e ->
      List.iter
        ~f:
          (Tree_abstraction.Reasonligo.Errors.error_ppformat ~display_format ~no_colour f)
        e
    | `Cit_jsligo_tracer e ->
      List.iter
        ~f:(Tree_abstraction.Jsligo.Errors.error_ppformat ~display_format ~no_colour f)
        e
    | `Self_ast_imperative_tracer e ->
      Self_ast_imperative.Errors.error_ppformat ~display_format ~no_colour f e
    | `Desugaring_tracer e -> Desugaring.Errors.error_ppformat ~display_format f e
    | `Checking_tracer e -> Checking.Errors.error_ppformat ~display_format ~no_colour f e
    | `Self_ast_typed_tracer e ->
      Self_ast_typed.Errors.error_ppformat ~display_format ~no_colour f e
    | `Aggregation_tracer e ->
      Aggregation.Errors.error_ppformat ~display_format ~no_colour f e
    | `Self_ast_aggregated_tracer e ->
      Self_ast_aggregated.Errors.error_ppformat ~display_format ~no_colour f e
    | `Self_mini_c_tracer e ->
      Self_mini_c.Errors.error_ppformat ~display_format ~no_colour f e
    | `Spilling_tracer e -> Spilling.Errors.error_ppformat ~display_format ~no_colour f e
    | `Scoping_tracer e -> Scoping.Errors.error_ppformat ~display_format f e
    | `Stacking_tracer e -> Stacking.Errors.error_ppformat ~display_format f e
    | `Main_interpret_not_enough_initial_accounts (loc, max) ->
      Format.fprintf
        f
        "@[<hv>%a@. baker account initial balance must at least reach %a tez @]"
        snippet_pp
        loc
        Memory_proto_alpha.Protocol.Alpha_context.Tez.pp
        max
    | `Main_interpret_test_entry_not_found s ->
      Format.fprintf f "Test entry '%s' not found" s
    | `Main_interpret_target_lang_error (loc, [], errs) ->
      Format.fprintf
        f
        "@[<v 4>%a@.An uncaught error occured:@.%a@]"
        snippet_pp
        loc
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
    | `Main_interpret_target_lang_error (loc, calltrace, errs) ->
      if (not (is_dummy_location loc)) || List.is_empty calltrace
      then
        Format.fprintf
          f
          "@[<v 4>%a@.An uncaught error occured:@.%a@.Trace:@.%a@]"
          snippet_pp
          loc
          (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
             ~details:true
             ~show_source:true
             ?parsed:None)
          errs
          (PP_helpers.list_sep_d Location.pp)
          calltrace
      else
        Format.fprintf
          f
          "@[<v 4>%a@.An uncaught error occured:@.%a@.Trace:@.%a@.%a@]"
          snippet_pp
          loc
          (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
             ~details:true
             ~show_source:true
             ?parsed:None)
          errs
          snippet_pp
          (List.hd_exn calltrace)
          (PP_helpers.list_sep_d Location.pp)
          (List.tl_exn calltrace)
    | `Main_interpret_target_lang_failwith (loc, [], v) ->
      Format.fprintf
        f
        "@[<v 4>%a@.An uncaught error occured:@.Failwith: %a@]"
        snippet_pp
        loc
        Tezos_utils.Michelson.pp
        v
    | `Main_interpret_target_lang_failwith (loc, calltrace, v) ->
      if (not (is_dummy_location loc)) || List.is_empty calltrace
      then
        Format.fprintf
          f
          "@[<v 4>%a@.An uncaught error occured:@.Failwith: %a@.Trace:@.%a@]"
          snippet_pp
          loc
          Tezos_utils.Michelson.pp
          v
          (PP_helpers.list_sep_d Location.pp)
          calltrace
      else
        Format.fprintf
          f
          "@[<v 4>%a@.An uncaught error occured:@.Failwith: %a@.Trace:@.%a@.%a@]"
          snippet_pp
          loc
          Tezos_utils.Michelson.pp
          v
          snippet_pp
          (List.hd_exn calltrace)
          (PP_helpers.list_sep_d Location.pp)
          (List.tl_exn calltrace)
    | `Main_interpret_boostrap_not_enough loc ->
      Format.fprintf
        f
        "@[<hv>%a@.We need at least two boostrap accounts for the default source and \
         baker@]"
        snippet_pp
        loc
    | `Main_interpret_meta_lang_eval (loc, [], reason) ->
      Format.fprintf f "@[<hv>%a@.%a@]" snippet_pp loc pp_value reason
    | `Main_interpret_meta_lang_eval (loc, calltrace, reason) ->
      if (not (is_dummy_location loc)) || List.is_empty calltrace
      then
        Format.fprintf
          f
          "@[<hv>%a@.%a@.Trace:@.%a@]"
          snippet_pp
          loc
          pp_value
          reason
          (PP_helpers.list_sep_d Location.pp)
          calltrace
      else
        Format.fprintf
          f
          "@[<hv>%a@.%a@.Trace:@.%a@.%a@]"
          snippet_pp
          loc
          pp_value
          reason
          snippet_pp
          (List.hd_exn calltrace)
          (PP_helpers.list_sep_d Location.pp)
          (List.tl_exn calltrace)
    | `Main_interpret_meta_lang_failwith (loc, [], value) ->
      Format.fprintf f "@[<hv>%a@.Test failed with %a@]" snippet_pp loc pp_value value
    | `Main_interpret_meta_lang_failwith (loc, calltrace, value) ->
      if (not (is_dummy_location loc)) || List.is_empty calltrace
      then
        Format.fprintf
          f
          "@[<hv>%a@.Test failed with %a@.Trace:@.%a@]"
          snippet_pp
          loc
          pp_value
          value
          (PP_helpers.list_sep_d Location.pp)
          calltrace
      else
        Format.fprintf
          f
          "@[<hv>%a@.Test failed with %a@.Trace:@.%a@.%a@]"
          snippet_pp
          loc
          pp_value
          value
          snippet_pp
          (List.hd_exn calltrace)
          (PP_helpers.list_sep_d Location.pp)
          (List.tl_exn calltrace)
    | `Main_interpret_generic (loc, desc) ->
      Format.fprintf f "@[<hv>%a@.%s@]" snippet_pp loc desc
    | `Main_interpret_literal (loc, l) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid interpretation of literal: %a@]"
        snippet_pp
        loc
        Ligo_prim.Literal_value.pp
        l
    | `Main_interpret_modules_not_supported loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Module are not handled in interpreter yet@]"
        snippet_pp
        loc
    | `Main_decompile_michelson e -> Stacking.Errors.error_ppformat ~display_format f e
    | `Main_decompile_mini_c e ->
      Spilling.Errors.error_ppformat ~display_format ~no_colour f e
    | `Main_decompile_typed e ->
      Checking.Errors.error_ppformat ~display_format ~no_colour f e
    | `Main_view_rule_violated loc ->
      Format.fprintf
        f
        "@[<hv>%a@.View rule violated:\n\
        \      - Tezos.create_contract ; Tezos.set_delegate and Tezos.transaction cannot \
         be used because they are stateful (expect in lambdas)\n\
        \      - Tezos.self can't be used because the entry-point does not make sense in \
         a view@.@]"
        snippet_pp
        loc
    | `Repl_unexpected -> Format.fprintf f "unexpected error, missing expression?"
    | `Ligo_init_unrecognized_template lststr ->
      Format.fprintf
        f
        "Error: Unrecognized template\n\
         Hint: Use the option --template \"TEMPLATE_NAME\" \n\n\
         Please select a template from the following list: \n\
         - %s\n\
         Or check if template exists on LIGO registry.\n"
      @@ String.concat ~sep:"\n- " lststr
    | `Ligo_init_registry_template_error e -> Format.fprintf f "@[<hv>@.%s@.@]" e
    | `Ligo_init_git_template_error e -> Format.fprintf f "@[<hv>@.%s@.@]" e)


let rec error_json : Types.all -> Simple_utils.Error.t list =
 fun a ->
  let open Simple_utils.Error in
  match a with
  | `Test_err_tracer (name, err) ->
    let children = error_json err in
    let message = name in
    let errors =
      List.map children ~f:(fun children ->
          let content = make_content ~message ~children () in
          make ~stage:"" ~content)
    in
    errors
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
  | `Test_repl _ ->
    let content = make_content ~message:"Test module tracer" () in
    [ make ~stage:"" ~content ]
  (* Top-level errors *)
  | `Build_error_tracer e -> [ BuildSystem.Errors.error_json e ]
  | `Main_invalid_generator_name _ ->
    let content = make_content ~message:"bad generator name" () in
    [ make ~stage:"command line interpreter" ~content ]
  | `Main_invalid_syntax_name _ ->
    let content = make_content ~message:"bad syntax name" () in
    [ make ~stage:"command line interpreter" ~content ]
  | `Main_invalid_dialect_name _ ->
    let content = make_content ~message:"bad dialect name" () in
    [ make ~stage:"command line interpreter" ~content ]
  | `Main_invalid_protocol_version _ ->
    let content = make_content ~message:"bad protocol version" () in
    [ make ~stage:"command line interpreter" ~content ]
  | `Main_invalid_extension _ ->
    let content = make_content ~message:"bad file extension" () in
    [ make ~stage:"command line interpreter" ~content ]
  | `Main_unparse_tracer _ ->
    let content = make_content ~message:"could not unparse michelson type" () in
    [ make ~stage:"michelson contract build" ~content ]
  | `Main_typecheck_contract_tracer (_p, _c, _) ->
    let content = make_content ~message:"Could not typecheck michelson code" () in
    [ make ~stage:"michelson contract build" ~content ]
  | `Main_could_not_serialize _errs ->
    let content = make_content ~message:"Could not serialize michelson code" () in
    [ make ~stage:"michelson serialization" ~content ]
  | `Check_typed_arguments_tracer (Simple_utils.Runned_result.Check_parameter, err) ->
    let children = error_json err in
    let message = "Passed parameter does not match the contract type" in
    let errors =
      List.map children ~f:(fun children ->
          let content = make_content ~message ~children () in
          make ~stage:"contract argument typechecking" ~content)
    in
    errors
  | `Check_typed_arguments_tracer (Simple_utils.Runned_result.Check_storage, err) ->
    let children = error_json err in
    let message = "Passed storage does not match the contract type" in
    let errors =
      List.map children ~f:(fun children ->
          let content = make_content ~message ~children () in
          make ~stage:"contract argument typechecking" ~content)
    in
    errors
  | `Main_unknown ->
    let content = make_content ~message:"unknown error" () in
    [ make ~stage:"michelson execution" ~content ]
  | `Main_execution_failed _ ->
    let content = make_content ~message:"main execution failed" () in
    [ make ~stage:"michelson execution" ~content ]
  | `Main_invalid_amount _ ->
    let content = make_content ~message:"invalid amount" () in
    [ make ~stage:"parsing command line parameters" ~content ]
  | `Main_invalid_balance _ ->
    let content = make_content ~message:"invalid balance" () in
    [ make ~stage:"parsing command line parameters" ~content ]
  | `Main_invalid_source _ ->
    let content = make_content ~message:"invalid source" () in
    [ make ~stage:"parsing command line parameters" ~content ]
  | `Main_invalid_sender _ ->
    let content = make_content ~message:"invalid sender" () in
    [ make ~stage:"parsing command line parameters" ~content ]
  | `Main_invalid_timestamp _ ->
    let content = make_content ~message:"invalid timestamp notation" () in
    [ make ~stage:"parsing command line parameters" ~content ]
  | `Main_cannot_open_global_constants _ ->
    let content = make_content ~message:"cannot open global constants file" () in
    [ make ~stage:"global constants parsing" ~content ]
  | `Main_cannot_parse_global_constants _ ->
    let content = make_content ~message:"cannot parse global constants file" () in
    [ make ~stage:"global constants parsing" ~content ]
  | `Unparsing_michelson_tracer _ ->
    let content = make_content ~message:"error unparsing michelson result" () in
    [ make ~stage:"michelson execution" ~content ]
  | `Parsing_payload_tracer _ ->
    let content = make_content ~message:"error parsing message" () in
    [ make ~stage:"michelson execution" ~content ]
  | `Packing_payload_tracer _ ->
    let content = make_content ~message:"error packing message" () in
    [ make ~stage:"michelson execution" ~content ]
  | `Parsing_input_tracer _ ->
    let content = make_content ~message:"error parsing input" () in
    [ make ~stage:"michelson execution" ~content ]
  | `Parsing_code_tracer _ ->
    let content = make_content ~message:"error parsing program code" () in
    [ make ~stage:"michelson execution" ~content ]
  | `Error_of_execution_tracer _ ->
    let content = make_content ~message:"error of execution" () in
    [ make ~stage:"michelson execution" ~content ]
  | `Main_entrypoint_not_a_function ->
    let content = make_content ~message:"given entrypoint is not a function" () in
    [ make ~stage:"top-level glue" ~content ]
  | `Main_view_not_a_function _str ->
    let content = make_content ~message:"given view is not a function" () in
    [ make ~stage:"top-level glue" ~content ]
  | `Main_view_rule_violated location ->
    let content = make_content ~message:"view rule violated" ~location () in
    [ make ~stage:"top-level glue" ~content ]
  | `Main_entrypoint_not_found ->
    let content = make_content ~message:"Missing entrypoint" () in
    [ make ~stage:"top-level glue" ~content ]
  | `Preproc_tracer e -> [ Preprocessing.Errors.error_json e ]
  | `Parser_tracer e -> [ Parsing.Errors.error_json e ]
  | `Pretty_tracer _ ->
    let content = make_content ~message:"Pretty printing tracer" () in
    [ make ~stage:"pretty" ~content ]
  | `Cit_pascaligo_tracer e -> List.map ~f:Tree_abstraction.Pascaligo.Errors.error_json e
  | `Cit_cameligo_tracer e -> List.map ~f:Tree_abstraction.Cameligo.Errors.error_json e
  | `Cit_reasonligo_tracer e ->
    List.map ~f:Tree_abstraction.Reasonligo.Errors.error_json e
  | `Cit_jsligo_tracer e -> List.map ~f:Tree_abstraction.Jsligo.Errors.error_json e
  | `Self_ast_imperative_tracer e -> [ Self_ast_imperative.Errors.error_json e ]
  | `Desugaring_tracer e -> [ Desugaring.Errors.error_json e ]
  | `Checking_tracer e -> [ Checking.Errors.error_json e ]
  | `Self_ast_typed_tracer e -> [ Self_ast_typed.Errors.error_json e ]
  | `Aggregation_tracer e -> [ Aggregation.Errors.error_json e ]
  | `Self_ast_aggregated_tracer e -> [ Self_ast_aggregated.Errors.error_json e ]
  | `Spilling_tracer e -> [ Spilling.Errors.error_json e ]
  | `Self_mini_c_tracer e -> [ Self_mini_c.Errors.error_json e ]
  | `Scoping_tracer e -> [ Scoping.Errors.error_json e ]
  | `Stacking_tracer e -> [ Stacking.Errors.error_json e ]
  | `Main_interpret_test_entry_not_found _
  | `Main_interpret_target_lang_error _
  | `Main_interpret_target_lang_failwith _
  | `Main_interpret_boostrap_not_enough _
  | `Main_interpret_meta_lang_eval _
  | `Main_interpret_meta_lang_failwith _
  | `Main_interpret_generic _
  | `Main_interpret_literal _
  | `Main_interpret_modules_not_supported _
  | `Main_interpret_not_enough_initial_accounts _ ->
    let content = make_content ~message:"Intrepret tracer" () in
    [ make ~stage:"testing framwork" ~content ]
  | `Main_decompile_michelson e -> [ Stacking.Errors.error_json e ]
  | `Main_decompile_mini_c e -> [ Spilling.Errors.error_json e ]
  | `Main_decompile_typed e -> [ Checking.Errors.error_json e ]
  | `Ligo_init_unrecognized_template _lsttr ->
    let content = make_content ~message:"Ligo init tracer" () in
    [ make ~stage:"init" ~content ]
  | `Ligo_init_registry_template_error _ ->
    let content = make_content ~message:"Ligo init tracer" () in
    [ make ~stage:"init" ~content ]
  | `Ligo_init_git_template_error _ ->
    let content = make_content ~message:"Ligo init tracer" () in
    [ make ~stage:"init" ~content ]
  | `Repl_unexpected ->
    let content = make_content ~message:"REPL tracer" () in
    [ make ~stage:"repl" ~content ]


let error_jsonformat : Types.all -> Yojson.Safe.t =
 fun e ->
  let errors = error_json e in
  let errors = List.map errors ~f:Simple_utils.Error.to_yojson in
  `List errors


let error_format : _ Simple_utils.Display.format =
  { pp = error_ppformat; to_json = error_jsonformat }
