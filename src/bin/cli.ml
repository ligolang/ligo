open Cmdliner
open Trace
open Cli_helpers

let main =
  let term = Term.(const print_endline $ const "Ligo needs a command. Do ligo --help") in
  (term , Term.info "ligo")

let source n =
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

let compile_file =
  let f source entry_point syntax =
    toplevel @@
    let%bind contract =
      trace (simple_info "compiling contract to michelson") @@
      Ligo.Run.compile_contract_file source entry_point (Syntax_name syntax) in
    Format.printf "%s\n" contract ;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ syntax) in
  let cmdname = "compile-contract" in
  let docs = "Subcommand: compile a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let compile_parameter =
  let f source entry_point expression syntax =
    toplevel @@
    let%bind value =
      trace (simple_error "compile-input") @@
      Ligo.Run.compile_contract_parameter source entry_point expression (Syntax_name syntax) in
    Format.printf "%s\n" value;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ expression "PARAMETER" 2 $ syntax) in
  let cmdname = "compile-parameter" in
  let docs = "Subcommand: compile parameters to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which calls a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let compile_storage =
  let f source entry_point expression syntax bigmap =
    toplevel @@
    let%bind value =
      trace (simple_error "compile-storage") @@
      Ligo.Run.compile_contract_storage ?bigmap:(Some bigmap) source entry_point expression (Syntax_name syntax) in
    Format.printf "%s\n" value;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ expression "STORAGE" 2 $ syntax $ bigmap) in
  let cmdname = "compile-storage" in
  let docs = "Subcommand: compile an initial storage in ligo syntax to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which originates a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let dry_run =
  let f source entry_point storage input bigmap amount syntax =
    toplevel @@
    let%bind output =
      Ligo.Run.run_contract ~bigmap ~amount source entry_point storage input (Syntax_name syntax) in
    Format.printf "%a\n" Ast_simplified.PP.expression output ;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ expression "PARAMETER" 2 $ expression "STORAGE" 3 $ bigmap $ amount $ syntax) in
  let cmdname = "dry-run" in
  let docs = "Subcommand: run a smart-contract with the given storage and input." in
  (term , Term.info ~docs cmdname)

let run_function =
  let f source entry_point parameter amount syntax =
    toplevel @@
    let%bind output =
      Ligo.Run.run_function ~amount source entry_point parameter (Syntax_name syntax) in
    Format.printf "%a\n" Ast_simplified.PP.expression output ;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ expression "PARAMETER" 2 $ amount $ syntax) in
  let cmdname = "run-function" in
  let docs = "Subcommand: run a function with the given parameter." in
  (term , Term.info ~docs cmdname)

let evaluate_value =
  let f source entry_point amount syntax =
    toplevel @@
    let%bind output =
      Ligo.Run.evaluate_value ~amount source entry_point (Syntax_name syntax) in
    Format.printf "%a\n" Ast_simplified.PP.expression output ;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ amount $ syntax) in
  let cmdname = "evaluate-value" in
  let docs = "Subcommand: evaluate a given definition." in
  (term , Term.info ~docs cmdname)


let () = Term.exit @@ Term.eval_choice main [
    compile_file ;
    compile_parameter ;
    compile_storage ;
    dry_run ;
    run_function ;
    evaluate_value ;
  ]
