open Cmdliner
open Trace

let toplevel x =
  match x with
  | Trace.Ok ((), annotations) -> ignore annotations; ()
  | Error ss ->
      Format.printf "%a%!" error_pp (ss ())

let main =
  let term = Term.(const print_endline $ const "Ligo needs a command. Do ligo --help") in
  (term , Term.info "ligo")

let source =
  let open Arg in
  let info =
    let docv = "SOURCE_FILE" in
    let doc = "$(docv) is the path to the .ligo file of the contract." in
    info ~docv ~doc [] in
  required @@ pos 0 (some string) None info

let entry_point =
  let open Arg in
  let info =
    let docv = "ENTRY_POINT" in
    let doc = "$(docv) is entry-point that will be compiled." in
    info ~docv ~doc [] in
  value @@ pos 1 string "main" info

let expression =
  let open Arg in
  let docv = "EXPRESSION" in
  let doc = "$(docv) is the expression that will be compiled." in
  let info = info ~docv ~doc [] in
  required @@ pos 2 (some string) None info

let syntax =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\" and \"cameligo\". \"pascaligo\" is the default." in
    info ~docv ~doc [] in
  value @@ opt string "pascaligo" info

let compile_file =
  let f source entry_point syntax =
    toplevel @@
    let%bind contract =
      trace (simple_info "compiling contract to michelson") @@
      Ligo.Run.compile_contract_file source entry_point syntax in
    Format.printf "Contract:\n%s\n" contract ;
    ok ()
  in
  let term =
    Term.(const f $ source $ entry_point $ syntax) in
  let cmdname = "compile-contract" in
  let docs = "Subcommand: compile a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let compile_parameter =
  let f source entry_point expression syntax =
    toplevel @@
    let%bind value =
      trace (simple_error "compile-input") @@
      Ligo.Run.compile_contract_parameter source entry_point expression syntax in
    Format.printf "Input:\n%s\n" value;
    ok ()
  in
  let term =
    Term.(const f $ source $ entry_point $ expression $ syntax) in
  let cmdname = "compile-parameter" in
  let docs = "Subcommand: compile parameters to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which calls a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let compile_storage =
  let f source entry_point expression syntax =
    toplevel @@
    let%bind value =
      trace (simple_error "compile-storage") @@
      Ligo.Run.compile_contract_storage source entry_point expression syntax in
    Format.printf "Storage:\n%s\n" value;
    ok ()
  in
  let term =
    Term.(const f $ source $ entry_point $ expression $ syntax) in
  let cmdname = "compile-storage" in
  let docs = "Subcommand: compile an initial storage in ligo syntax to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which originates a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)


let () = Term.exit @@ Term.eval_choice main [compile_file ; compile_parameter ; compile_storage]
