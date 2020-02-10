open Cmdliner
open Trace
open Cli_helpers

let version =
  Format.asprintf
    "Rolling release\nHash: %s\nDate: %s\nCI job id: %s"
    Version.hash
    Version.commit_date
    Version.job_id

let main =
  let man =
    [ `S "MORE HELP";
      `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command." ] in
  (Term.(ret (const (`Help (`Auto, None)))), Term.info "ligo" ~version ~man)

let source_file n =
  let open Arg in
  let info =
    let docv = "SOURCE_FILE" in
    let doc = "$(docv) is the path to the smart contract file." in
    info ~docv ~doc [] in
  required @@ pos n (some string) None info

let entry_point n =
  let open Arg in
  let info =
    let docv = "ENTRY_POINT" in
    let doc = "$(docv) is entry-point that will be compiled." in
    info ~docv ~doc [] in
  required @@ pos n (some string) (Some "main") info

let expression purpose n =
  let open Arg in
  let docv = purpose ^ "_EXPRESSION" in
  let doc = "$(docv) is the expression that will be compiled." in
  let info = info ~docv ~doc [] in
  required @@ pos n (some string) None info

let syntax =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo respectively)." in
    info ~docv ~doc ["syntax" ; "s"] in
  value @@ opt string "auto" info

let req_syntax n =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo respectively)." in
    info ~docv ~doc [] in
  required @@ pos n (some string) None info

let init_file =
  let open Arg in
  let info =
    let docv = "INIT_FILE" in
    let doc = "$(docv) is the path to smart contract file to be used for context initialization." in
    info ~docv ~doc ["init-file"] in
  value @@ opt (some string) None info

let amount =
  let open Arg in
  let info =
    let docv = "AMOUNT" in
    let doc = "$(docv) is the amount the Michelson interpreter will use." in
    info ~docv ~doc ["amount"] in
  value @@ opt string "0" info

let sender =
  let open Arg in
  let info =
    let docv = "SENDER" in
    let doc = "$(docv) is the sender the Michelson interpreter transaction will use." in
    info ~docv ~doc ["sender"] in
  value @@ opt (some string) None info

let source =
  let open Arg in
  let info =
    let docv = "SOURCE" in
    let doc = "$(docv) is the source the Michelson interpreter transaction will use." in
    info ~docv ~doc ["source"] in
  value @@ opt (some string) None info

let predecessor_timestamp =
  let open Arg in
  let info =
    let docv = "PREDECESSOR_TIMESTAMP" in
    let doc = "$(docv) is the predecessor_timestamp (now value minus one minute) the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')" in
    info ~docv ~doc ["predecessor-timestamp"] in
  value @@ opt (some string) None info

let display_format =
  let open Arg in
  let info  =
    let docv = "DISPLAY_FORMAT" in
    let doc = "$(docv) is the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile." in
    info ~docv ~doc ["format" ; "display-format"] in
  value @@
  opt
    (enum [("human-readable", `Human_readable); ("dev", `Dev); ("json", `Json)])
    `Human_readable
    info

let michelson_code_format =
  let open Arg in
  let info  =
    let docv = "MICHELSON_FORMAT" in
    let doc = "$(docv) is the format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'." in
    info ~docv ~doc ["michelson-format"] in
  value @@
  opt
    (enum [("text", `Text); ("json", `Json); ("hex", `Hex)])
    `Text info

module Helpers = Ligo.Compile.Helpers
module Compile = Ligo.Compile
module Uncompile = Ligo.Uncompile
module Run = Ligo.Run.Of_michelson

let compile_file =
  let f source_file entry_point syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind simplified = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed,_    = Compile.Of_simplified.compile simplified in
    let%bind mini_c     = Compile.Of_typed.compile typed in
    let%bind michelson  = Compile.Of_mini_c.aggregate_and_compile_contract mini_c entry_point in
    let%bind contract   = Compile.Of_michelson.build_contract michelson in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) contract
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ syntax $ display_format $ michelson_code_format) in
  let cmdname = "compile-contract" in
  let doc = "Subcommand: Compile a contract." in
  (Term.ret term , Term.info ~doc cmdname)

let print_cst =
  let f source_file syntax display_format = (
    toplevel ~display_format @@
    let%bind pp = Compile.Of_source.pretty_print source_file (Syntax_name syntax) in
    ok @@ Format.asprintf "%s \n" (Buffer.contents pp)
  )
  in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "print-cst" in
  let doc = "Subcommand: Print the CST.\nWarning: Intended for development of LIGO and can break at any time." in
  (Term.ret term, Term.info ~doc cmdname)

let print_ast =
  let f source_file syntax display_format  = (
    toplevel ~display_format @@
    let%bind simplified = Compile.Of_source.compile source_file (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" Compile.Of_simplified.pretty_print simplified
  )
  in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "print-ast" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  (Term.ret term, Term.info ~doc cmdname)

let print_typed_ast =
  let f source_file syntax display_format  = (
    toplevel ~display_format @@
    let%bind simplified = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed,_    = Compile.Of_simplified.compile simplified in
    ok @@ Format.asprintf "%a\n" Compile.Of_typed.pretty_print typed
  )
  in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "print-typed-ast" in
  let doc = "Subcommand: Print the typed AST.\n Warning: Intended for development of LIGO and can break at any time." in
  (Term.ret term, Term.info ~doc cmdname)

let print_mini_c =
  let f source_file syntax display_format  = (
    toplevel ~display_format @@
    let%bind simplified = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed,_    = Compile.Of_simplified.compile simplified in
    let%bind mini_c     = Compile.Of_typed.compile typed in
    ok @@ Format.asprintf "%a\n" Compile.Of_mini_c.pretty_print mini_c
  )
  in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "print-mini-c" in
  let doc = "Subcommand: Print Mini-C. Warning: Intended for development of LIGO and can break at any time." in
  (Term.ret term, Term.info ~doc cmdname)

let measure_contract =
  let f source_file entry_point syntax display_format  =
    toplevel ~display_format @@
    let%bind simplified = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed,_    = Compile.Of_simplified.compile simplified in
    let%bind mini_c     = Compile.Of_typed.compile typed in
    let%bind michelson  = Compile.Of_mini_c.aggregate_and_compile_contract mini_c entry_point in
    let%bind contract   = Compile.Of_michelson.build_contract michelson in
    let open Tezos_utils in
    ok @@ Format.asprintf "%d bytes\n" (Michelson.measure contract)
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ syntax $ display_format) in
  let cmdname = "measure-contract" in
  let doc = "Subcommand: Measure a contract's compiled size in bytes." in
  (Term.ret term , Term.info ~doc cmdname)

let compile_parameter =
  let f source_file entry_point expression syntax amount sender source predecessor_timestamp display_format michelson_format =
    toplevel ~display_format @@
    let%bind simplified      = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed_prg,state = Compile.Of_simplified.compile simplified in
    let%bind mini_c_prg      = Compile.Of_typed.compile typed_prg in
    let%bind michelson_prg   = Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg entry_point in
    let      env             = Ast_typed.program_environment typed_prg in
    let%bind (_contract: Tezos_utils.Michelson.michelson) =
      (* fails if the given entry point is not a valid contract *)
      Compile.Of_michelson.build_contract michelson_prg in

    let%bind v_syntax         = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind simplified_param = Compile.Of_source.compile_expression v_syntax expression in
    let%bind (typed_param,_)  = Compile.Of_simplified.compile_expression ~env ~state simplified_param in
    let%bind mini_c_param     = Compile.Of_typed.compile_expression typed_param in
    let%bind compiled_param   = Compile.Of_mini_c.aggregate_and_compile_expression mini_c_prg mini_c_param in
    let%bind ()               = Compile.Of_typed.assert_equal_contract_type Check_parameter entry_point typed_prg typed_param in
    let%bind ()               = Compile.Of_michelson.assert_equal_contract_type Check_parameter michelson_prg compiled_param in
    let%bind options          = Run.make_dry_run_options {predecessor_timestamp ; amount ; sender ; source } in
    let%bind value            = Run.evaluate_expression ~options compiled_param.expr compiled_param.expr_ty in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) value
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ syntax $ amount $ sender $ source $ predecessor_timestamp $ display_format $ michelson_code_format) in
  let cmdname = "compile-parameter" in
  let doc = "Subcommand: Compile parameters to a Michelson expression. The resulting Michelson expression can be passed as an argument in a transaction which calls a contract." in
  (Term.ret term , Term.info ~doc cmdname)

let interpret =
  let f expression init_file syntax amount sender source predecessor_timestamp display_format =
    toplevel ~display_format @@
    let%bind (decl_list,state,env) = match init_file with
      | Some init_file ->
        let%bind simplified      = Compile.Of_source.compile init_file (Syntax_name syntax) in
        let%bind typed_prg,state = Compile.Of_simplified.compile simplified in
        let%bind mini_c_prg      = Compile.Of_typed.compile typed_prg in
        let      env             = Ast_typed.program_environment typed_prg in
        ok (mini_c_prg,state,env)
      | None -> ok ([],Typer.Solver.initial_state,Ast_typed.Environment.full_empty) in

    let%bind v_syntax       = Helpers.syntax_to_variant (Syntax_name syntax) init_file in
    let%bind simplified_exp = Compile.Of_source.compile_expression v_syntax expression in
    let%bind (typed_exp,_)  = Compile.Of_simplified.compile_expression ~env ~state simplified_exp in
    let%bind mini_c_exp     = Compile.Of_typed.compile_expression typed_exp in
    let%bind compiled_exp   = Compile.Of_mini_c.aggregate_and_compile_expression decl_list mini_c_exp in
    let%bind options        = Run.make_dry_run_options {predecessor_timestamp ; amount ; sender ; source } in
    let%bind runres         = Run.run_expression ~options compiled_exp.expr compiled_exp.expr_ty in
    match runres with
      | Fail fail_res ->
        let%bind failstring = Run.failwith_to_string fail_res in
        ok @@ Format.asprintf "%s" failstring
      | Success value' ->
        let%bind simplified_output = Uncompile.uncompile_expression typed_exp.type_expression value' in
        ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression simplified_output
  in
  let term =
    Term.(const f $ expression "EXPRESSION" 0 $ init_file $ syntax $ amount $ sender $ source $ predecessor_timestamp $ display_format ) in
  let cmdname = "interpret" in
  let doc = "Subcommand: Interpret the expression in the context initialized by the provided source file." in
  (Term.ret term , Term.info ~doc cmdname)

let temp_ligo_interpreter =
  let f source_file syntax display_format =
    toplevel ~display_format @@
    let%bind simplified = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed,_    = Compile.Of_simplified.compile simplified in
    let%bind res = Compile.Of_typed.some_interpret typed in
    ok @@ Format.asprintf "%s\n" res
  in
  let term =
    Term.(const f $ source_file 0 $ syntax $ display_format ) in
  let cmdname = "ligo-interpret" in
  let doc = "Subcommand: (temporary / dev only) uses LIGO interpret." in
  (Term.ret term , Term.info ~doc cmdname)

let compile_storage =
  let f source_file entry_point expression syntax amount sender source predecessor_timestamp display_format michelson_format =
    toplevel ~display_format @@
    let%bind simplified      = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed_prg,state = Compile.Of_simplified.compile simplified in
    let%bind mini_c_prg      = Compile.Of_typed.compile typed_prg in
    let%bind michelson_prg   = Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg entry_point in
    let      env             = Ast_typed.program_environment typed_prg in
    let%bind (_contract: Tezos_utils.Michelson.michelson) =
      (* fails if the given entry point is not a valid contract *)
      Compile.Of_michelson.build_contract michelson_prg in

    let%bind v_syntax         = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind simplified_param = Compile.Of_source.compile_expression v_syntax expression in
    let%bind (typed_param,_)  = Compile.Of_simplified.compile_expression ~env ~state simplified_param in
    let%bind mini_c_param     = Compile.Of_typed.compile_expression typed_param in
    let%bind compiled_param   = Compile.Of_mini_c.aggregate_and_compile_expression mini_c_prg mini_c_param in
    let%bind ()               = Compile.Of_typed.assert_equal_contract_type Check_storage entry_point typed_prg typed_param in
    let%bind ()               = Compile.Of_michelson.assert_equal_contract_type Check_storage michelson_prg compiled_param in
    let%bind options          = Run.make_dry_run_options {predecessor_timestamp ; amount ; sender ; source } in
    let%bind value            = Run.evaluate_expression ~options compiled_param.expr compiled_param.expr_ty in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) value
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "STORAGE" 2 $ syntax $ amount $ sender $ source $ predecessor_timestamp $ display_format $ michelson_code_format) in
  let cmdname = "compile-storage" in
  let doc = "Subcommand: Compile an initial storage in ligo syntax to a Michelson expression. The resulting Michelson expression can be passed as an argument in a transaction which originates a contract." in
  (Term.ret term , Term.info ~doc cmdname)

let dry_run =
  let f source_file entry_point storage input amount sender source predecessor_timestamp syntax display_format =
    toplevel ~display_format @@
    let%bind simplified      = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed_prg,state = Compile.Of_simplified.compile simplified in
    let      env             = Ast_typed.program_environment typed_prg in
    let%bind mini_c_prg      = Compile.Of_typed.compile typed_prg in
    let%bind michelson_prg   = Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg entry_point in
    let%bind (_contract: Tezos_utils.Michelson.michelson) =
      (* fails if the given entry point is not a valid contract *)
      Compile.Of_michelson.build_contract michelson_prg in

    let%bind v_syntax          = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind simplified        = Compile.Of_source.compile_contract_input storage input v_syntax in
    let%bind typed,_           = Compile.Of_simplified.compile_expression ~env ~state simplified in
    let%bind mini_c            = Compile.Of_typed.compile_expression typed in
    let%bind compiled_params   = Compile.Of_mini_c.compile_expression mini_c in
    let%bind args_michelson    = Run.evaluate_expression compiled_params.expr compiled_params.expr_ty in

    let%bind options           = Run.make_dry_run_options {predecessor_timestamp ; amount ; sender ; source } in
    let%bind runres  = Run.run_contract ~options michelson_prg.expr michelson_prg.expr_ty args_michelson in
    match runres with
      | Fail fail_res ->
        let%bind failstring = Run.failwith_to_string fail_res in
        ok @@ Format.asprintf "%s" failstring
      | Success michelson_output ->
        let%bind simplified_output = Uncompile.uncompile_typed_program_entry_function_result typed_prg entry_point michelson_output in
        ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression simplified_output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ expression "STORAGE" 3 $ amount $ sender $ source $ predecessor_timestamp $ syntax $ display_format) in
  let cmdname = "dry-run" in
  let doc = "Subcommand: Run a smart-contract with the given storage and input." in
  (Term.ret term , Term.info ~doc cmdname)

let run_function =
  let f source_file entry_point parameter amount sender source predecessor_timestamp syntax display_format =
    toplevel ~display_format @@
    let%bind v_syntax        = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind simplified_prg  = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed_prg,state = Compile.Of_simplified.compile simplified_prg in
    let      env             = Ast_typed.program_environment typed_prg in
    let%bind mini_c_prg      = Compile.Of_typed.compile typed_prg in


    let%bind simplified_param = Compile.Of_source.compile_expression v_syntax parameter in
    let%bind app              = Compile.Of_simplified.apply entry_point simplified_param in
    let%bind (typed_app,_)    = Compile.Of_simplified.compile_expression ~env ~state app in
    let%bind compiled_applied = Compile.Of_typed.compile_expression typed_app in

    let%bind michelson        = Compile.Of_mini_c.aggregate_and_compile_expression mini_c_prg compiled_applied in
    let%bind options          = Run.make_dry_run_options {predecessor_timestamp ; amount ; sender ; source } in
    let%bind runres           = Run.run_expression ~options michelson.expr michelson.expr_ty in
    match runres with
      | Fail fail_res ->
        let%bind failstring = Run.failwith_to_string fail_res in
        ok @@ Format.asprintf "%s" failstring
      | Success michelson_output ->
        let%bind simplified_output = Uncompile.uncompile_typed_program_entry_function_result typed_prg entry_point michelson_output in
        ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression simplified_output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ amount $ sender $ source $ predecessor_timestamp $ syntax $ display_format) in
  let cmdname = "run-function" in
  let doc = "Subcommand: Run a function with the given parameter." in
  (Term.ret term , Term.info ~doc cmdname)

let evaluate_value =
  let f source_file entry_point amount sender source predecessor_timestamp syntax display_format =
    toplevel ~display_format @@
    let%bind simplified        = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let%bind typed_prg,_       = Compile.Of_simplified.compile simplified in
    let%bind mini_c            = Compile.Of_typed.compile typed_prg in
    let%bind (exp,_)           = Mini_c.get_entry mini_c entry_point in
    let%bind compiled          = Compile.Of_mini_c.aggregate_and_compile_expression mini_c exp in
    let%bind options           = Run.make_dry_run_options {predecessor_timestamp ; amount ; sender ; source } in
    let%bind michelson_output  = Run.run_no_failwith ~options compiled.expr compiled.expr_ty in
    let%bind simplified_output = Uncompile.uncompile_typed_program_entry_expression_result typed_prg entry_point michelson_output in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression simplified_output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ amount $ sender $ source $ predecessor_timestamp $ syntax $ display_format) in
  let cmdname = "evaluate-value" in
  let doc = "Subcommand: Evaluate a given definition." in
  (Term.ret term , Term.info ~doc cmdname)

let compile_expression =
  let f expression syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind v_syntax = Helpers.syntax_to_variant (Syntax_name syntax) (None) in
    let      env      = Ast_typed.Environment.full_empty in
    let      state    = Typer.Solver.initial_state in
    let%bind simplified    = Compile.Of_source.compile_expression v_syntax expression in
    let%bind (typed_exp,_) = Compile.Of_simplified.compile_expression ~env ~state simplified in
    let%bind mini_c_exp    = Compile.Of_typed.compile_expression typed_exp in
    let%bind compiled_exp  = Compile.Of_mini_c.compile_expression mini_c_exp in
    let%bind value         = Run.evaluate_expression compiled_exp.expr compiled_exp.expr_ty in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) value
  in
  let term =
    Term.(const f $ expression "" 1 $ req_syntax 0 $ display_format $ michelson_code_format) in
  let cmdname = "compile-expression" in
  let doc = "Subcommand: Compile to a michelson value." in
  (Term.ret term , Term.info ~doc cmdname)

let dump_changelog =
  let f display_format = toplevel ~display_format @@ (ok @@ [%blob "../../CHANGELOG.md"]) in
  let term =
    Term.(const f $ display_format) in
  let cmdname = "changelog" in
  let doc = "Dump the LIGO changelog to stdout." in
  (Term.ret term , Term.info ~doc cmdname)

let list_declarations =
  let f source_file syntax =
    toplevel ~display_format:(`Human_readable) @@
    let%bind simplified_prg  = Compile.Of_source.compile source_file (Syntax_name syntax) in
    let json_decl = List.map (fun decl -> `String decl) @@ Compile.Of_simplified.list_declarations simplified_prg in
    ok @@ J.to_string @@ `Assoc [ ("source_file", `String source_file) ; ("declarations", `List json_decl) ]
  in
  let term =
    Term.(const f $ source_file 0 $ syntax ) in
  let cmdname = "list-declarations" in
  let doc = "Subcommand: List all the top-level declarations." in
  (Term.ret term , Term.info ~doc cmdname)

let run ?argv () =
  Term.eval_choice ?argv main [
    temp_ligo_interpreter ;
    compile_file ;
    measure_contract ;
    compile_parameter ;
    compile_storage ;
    compile_expression ;
    interpret ;
    dry_run ;
    run_function ;
    evaluate_value ;
    dump_changelog ;
    print_cst ;
    print_ast ;
    print_typed_ast ;
    print_mini_c ;
    list_declarations ;
  ]
