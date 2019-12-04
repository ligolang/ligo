open Cmdliner
open Trace
open Cli_helpers

let main =
  let man =
    [ `S "MORE HELP";
      `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command." ] in
  (Term.(ret (const (`Help (`Auto, None)))), Term.info "ligo" ~man)

let source_file n =
  let open Arg in
  let info =
    let docv = "SOURCE_FILE" in
    let doc = "$(docv) is the path to the .ligo or .mligo file of the contract." in
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
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\" and \"cameligo\". By default, the syntax is guessed from the extension (.ligo and .mligo, respectively)." in
    info ~docv ~doc ["syntax" ; "s"] in
  value @@ opt string "auto" info

let req_syntax n =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\" and \"cameligo\". By default, the syntax is guessed from the extension (.ligo and .mligo, respectively)." in
    info ~docv ~doc [] in
  required @@ pos n (some string) None info

let amount =
  let open Arg in
  let info =
    let docv = "AMOUNT" in
    let doc = "$(docv) is the amount the dry-run transaction will use." in
    info ~docv ~doc ["amount"] in
  value @@ opt string "0" info

let sender =
  let open Arg in
  let info =
    let docv = "SENDER" in
    let doc = "$(docv) is the sender the dry-run transaction will use." in
    info ~docv ~doc ["sender"] in
  value @@ opt (some string) None info

let source =
  let open Arg in
  let info =
    let docv = "SOURCE" in
    let doc = "$(docv) is the source the dry-run transaction will use." in
    info ~docv ~doc ["source"] in
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
module Compile = Ligo.Compile.Wrapper
module Uncompile = Ligo.Uncompile
module Run = Ligo.Run.Of_michelson

let compile_file =
  let f source_file entry_point syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind (contract,_) = Compile.source_to_michelson_contract (Syntax_name syntax) source_file entry_point in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) contract
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ syntax $ display_format $ michelson_code_format) in
  let cmdname = "compile-contract" in
  let doc = "Subcommand: compile a contract." in
  (term , Term.info ~doc cmdname)

let measure_contract =
  let f source_file entry_point syntax display_format  =
    toplevel ~display_format @@
    let%bind (contract,_) = Compile.source_to_michelson_contract (Syntax_name syntax) source_file entry_point in
    let open Tezos_utils in
    ok @@ Format.asprintf "%d bytes\n" (Michelson.measure contract)
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ syntax $ display_format) in
  let cmdname = "measure-contract" in
  let doc = "Subcommand: measure a contract's compiled size in bytes." in
  (term , Term.info ~doc cmdname)

let compile_parameter =
  let f source_file _entry_point expression syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind v_syntax      = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind (_,state,env) = Compile.source_to_typed (Syntax_name syntax) source_file in
    let%bind compiled_exp  = Compile.source_expression_to_michelson_value_as_function ~env ~state expression v_syntax in
    let%bind value         = Run.evaluate_michelson compiled_exp in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) value
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ syntax $ display_format $ michelson_code_format) in
  let cmdname = "compile-parameter" in
  let doc = "Subcommand: compile parameters to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which calls a contract." in
  (term , Term.info ~doc cmdname)

let compile_storage =
  let f source_file _entry_point expression syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind v_syntax      = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind (_,state,env) = Compile.source_to_typed (Syntax_name syntax) source_file in
    let%bind compiled      = Compile.source_expression_to_michelson_value_as_function ~env ~state expression v_syntax in
    let%bind value         = Run.evaluate_michelson compiled in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) value
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "STORAGE" 2 $ syntax $ display_format $ michelson_code_format) in
  let cmdname = "compile-storage" in
  let doc = "Subcommand: compile an initial storage in ligo syntax to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which originates a contract." in
  (term , Term.info ~doc cmdname)

let dry_run =
  let f source_file entry_point storage input amount sender source syntax display_format =
    toplevel ~display_format @@
    let%bind v_syntax                      = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind (_,(typed_program,state,env)) = Compile.source_to_michelson_contract (Syntax_name syntax) source_file entry_point in
    let%bind compiled_param                = Compile.source_contract_input_to_michelson_value_as_function ~env ~state (storage,input) v_syntax in
    let%bind michelson                     = Compile.typed_to_michelson_contract_as_exp typed_program entry_point in
    let%bind args_michelson                = Run.evaluate_michelson compiled_param in
    let%bind options                       = Run.make_dry_run_options {amount ; sender ; source } in
    let%bind michelson_output              = Run.run_contract ~options michelson.expr michelson.expr_ty args_michelson true in
    let%bind simplified_output             = Uncompile.uncompile_typed_program_entry_function_result typed_program entry_point michelson_output in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression simplified_output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ expression "STORAGE" 3 $ amount $ sender $ source $ syntax $ display_format) in
  let cmdname = "dry-run" in
  let doc = "Subcommand: run a smart-contract with the given storage and input." in
  (term , Term.info ~doc cmdname)

let run_function =
  let f source_file entry_point parameter amount sender source syntax display_format =
    toplevel ~display_format @@
    let%bind v_syntax                  = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind (typed_program,state,env) = Compile.source_to_typed (Syntax_name syntax) source_file in
    let%bind compiled_parameter        = Compile.source_expression_to_michelson_value_as_function ~env ~state parameter v_syntax in
    let%bind michelson                 = Compile.typed_to_michelson_program typed_program entry_point in
    let%bind args_michelson            = Run.evaluate_michelson compiled_parameter in
    let%bind options                   = Run.make_dry_run_options {amount ; sender ; source } in
    let%bind michelson_output          = Run.run ~options michelson args_michelson in
    let%bind simplified_output         = Uncompile.uncompile_typed_program_entry_function_result typed_program entry_point michelson_output in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression simplified_output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ amount $ sender $ source $ syntax $ display_format) in
  let cmdname = "run-function" in
  let doc = "Subcommand: run a function with the given parameter." in
  (term , Term.info ~doc cmdname)

let evaluate_value =
  let f source_file entry_point amount sender source syntax display_format =
    toplevel ~display_format @@
    let%bind (typed_program,_,_) = Compile.source_to_typed (Syntax_name syntax) source_file in
    let%bind contract            = Compile.typed_to_michelson_value_as_function typed_program entry_point in
    let%bind options             = Run.make_dry_run_options {amount ; sender ; source } in
    let%bind michelson_output    = Run.evaluate ~options contract in
    let%bind simplified_output   = Uncompile.uncompile_typed_program_entry_expression_result typed_program entry_point michelson_output in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression simplified_output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ amount $ sender $ source $ syntax $ display_format) in
  let cmdname = "evaluate-value" in
  let doc = "Subcommand: evaluate a given definition." in
  (term , Term.info ~doc cmdname)

let compile_expression =
  let f expression syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind v_syntax = Helpers.syntax_to_variant (Syntax_name syntax) (None) in
    let%bind compiled = Compile.source_expression_to_michelson_value_as_function
      ~env:(Ast_typed.Environment.full_empty) ~state:(Typer.Solver.initial_state)
      expression v_syntax in
    let%bind value    = Run.evaluate_michelson compiled in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) value
  in
  let term =
    Term.(const f $ expression "" 1 $ req_syntax 0 $ display_format $ michelson_code_format) in
  let cmdname = "compile-expression" in
  let doc = "Subcommand: compile to a michelson value." in
  (term , Term.info ~doc cmdname)

let run ?argv () =
  Term.eval_choice ?argv main [
    compile_file ;
    measure_contract ;
    compile_parameter ;
    compile_storage ;
    compile_expression ;
    dry_run ;
    run_function ;
    evaluate_value ;
  ]
