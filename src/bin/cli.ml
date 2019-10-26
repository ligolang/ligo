open Cmdliner
open Trace
open Cli_helpers

let main =
  let term = Term.(const print_endline $ const "Ligo needs a command. Do ligo --help") in
  (term , Term.info "ligo")

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

let bigmap =
  let open Arg in
  let info =
    let docv = "BIGMAP" in
    let doc = "$(docv) is necessary when your storage embeds a big_map." in
    info ~docv ~doc ["bigmap"] in
  value @@ flag info

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

let compile_file =
  let f source_file entry_point syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind contract =
      trace (simple_info "compiling contract to michelson") @@
      Ligo.Compile.Of_source.compile_file_contract_entry source_file entry_point (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) contract
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ syntax $ display_format $ michelson_code_format) in
  let cmdname = "compile-contract" in
  let docs = "Subcommand: compile a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let compile_parameter =
  let f source_file entry_point expression syntax display_format =
    toplevel ~display_format @@
    let%bind value =
      trace (simple_error "compile-input") @@
      Ligo.Run.Of_source.compile_file_contract_parameter source_file entry_point expression (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" Tezos_utils.Michelson.pp value
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ syntax $ display_format) in
  let cmdname = "compile-parameter" in
  let docs = "Subcommand: compile parameters to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which calls a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let compile_storage =
  let f source_file entry_point expression syntax display_format bigmap =
    toplevel ~display_format @@
    let%bind value =
      trace (simple_error "compile-storage") @@
      Ligo.Run.Of_source.compile_file_contract_storage ~value:bigmap source_file entry_point expression (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" Tezos_utils.Michelson.pp value
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "STORAGE" 2 $ syntax $ display_format $ bigmap) in
  let cmdname = "compile-storage" in
  let docs = "Subcommand: compile an initial storage in ligo syntax to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which originates a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let dry_run =
  let f source_file entry_point storage input amount sender source syntax display_format bigmap =
    toplevel ~display_format @@
    let%bind output =
      Ligo.Run.Of_source.run_contract
        ~options:{ amount ; sender ; source }
        ~storage_value:bigmap
        source_file entry_point storage input (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ expression "STORAGE" 3 $ amount $ sender $ source $ syntax $ display_format $ bigmap) in
  let cmdname = "dry-run" in
  let docs = "Subcommand: run a smart-contract with the given storage and input." in
  (term , Term.info ~docs cmdname)

let run_function =
  let f source_file entry_point parameter amount sender source syntax display_format =
    toplevel ~display_format @@
    let%bind output =
      Ligo.Run.Of_source.run_function_entry
        ~options:{ amount ; sender ; source }
        source_file entry_point parameter (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ amount $ sender $ source $ syntax $ display_format) in
  let cmdname = "run-function" in
  let docs = "Subcommand: run a function with the given parameter." in
  (term , Term.info ~docs cmdname)

let evaluate_value =
  let f source_file entry_point amount sender source syntax display_format =
    toplevel ~display_format @@
    let%bind output =
      Ligo.Run.Of_source.evaluate_entry
        ~options:{ amount ; sender ; source }
        source_file entry_point (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ amount $ sender $ source $ syntax $ display_format) in
  let cmdname = "evaluate-value" in
  let docs = "Subcommand: evaluate a given definition." in
  (term , Term.info ~docs cmdname)

let compile_expression =
  let f expression syntax display_format =
    toplevel ~display_format @@
    let%bind value =
      trace (simple_error "compile-input") @@
      Ligo.Run.Of_source.compile_expression expression (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" Tezos_utils.Michelson.pp value
  in
  let term =
    Term.(const f $ expression "" 0 $ syntax $ display_format) in
  let cmdname = "compile-expression" in
  let docs = "Subcommand: compile to a michelson value." in
  (term , Term.info ~docs cmdname)


let () = Term.exit @@ Term.eval_choice main [
    compile_file ;
    compile_parameter ;
    compile_storage ;
    compile_expression ;
    dry_run ;
    run_function ;
    evaluate_value ;
  ]
