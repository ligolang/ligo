open Cmdliner
open Trace

let error_pp out (e : error) =
    let open JSON_string_utils in
  let message =
    let opt = e |> member "message" |> string in
    let msg = Option.unopt ~default:"" opt in
    if msg = ""
    then ""
    else ": " ^ msg in
  let error_code =
    let error_code = e |> member "error_code" in
    match error_code with
    | `Null -> ""
    | _ -> " (" ^ (J.to_string error_code) ^ ")" in
  let title =
    let opt = e |> member "title" |> string in
    Option.unopt ~default:"" opt in
  let data =
    let data = e |> member "data" in
    match data with
    | `Null -> ""
    | _ -> " " ^ (J.to_string data) ^ "\n" in
  let infos =
    let infos = e |> member "infos" in
    match infos with
    | `Null -> ""
    | _ -> " " ^ (J.to_string infos) ^ "\n" in
  Format.fprintf out "%s%s%s.\n%s%s" title error_code message data infos


let toplevel x =
  match x with
  | Trace.Ok ((), annotations) -> ignore annotations; ()
  | Error ss -> (
      Format.printf "%a%!" error_pp (ss ())
    )

let main =
  let term = Term.(const print_endline $ const "Ligo needs a command. Do ligo --help") in
  (term , Term.info "ligo")

let source n =
  let open Arg in
  let info =
    let docv = "SOURCE_FILE" in
    let doc = "$(docv) is the path to the .ligo file of the contract." in
    info ~docv ~doc [] in
  required @@ pos n (some string) None info

let entry_point n =
  let open Arg in
  let info =
    let docv = "ENTRY_POINT" in
    let doc = "$(docv) is entry-point that will be compiled." in
    info ~docv ~doc [] in
  required @@ pos n (some string) (Some "main") info

let expression n =
  let open Arg in
  let docv = "EXPRESSION" in
  let doc = "$(docv) is the expression that will be compiled." in
  let info = info ~docv ~doc [] in
  required @@ pos n (some string) None info

let syntax =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\" and \"cameligo\". \"pascaligo\" is the default." in
    info ~docv ~doc ["syntax" ; "s"] in
  value @@ opt string "pascaligo" info

let compile_file =
  let f source entry_point syntax =
    toplevel @@
    let%bind contract =
      trace (simple_info "compiling contract to michelson") @@
      Ligo.Run.compile_contract_file source entry_point syntax in
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
      Ligo.Run.compile_contract_parameter source entry_point expression syntax in
    Format.printf "%s\n" value;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ expression 2 $ syntax) in
  let cmdname = "compile-parameter" in
  let docs = "Subcommand: compile parameters to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which calls a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let compile_storage =
  let f source entry_point expression syntax =
    toplevel @@
    let%bind value =
      trace (simple_error "compile-storage") @@
      Ligo.Run.compile_contract_storage source entry_point expression syntax in
    Format.printf "%s\n" value;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ expression 2 $ syntax) in
  let cmdname = "compile-storage" in
  let docs = "Subcommand: compile an initial storage in ligo syntax to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which originates a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let dry_run =
  let f source entry_point storage input syntax =
    toplevel @@
    let%bind output =
      Ligo.Run.run_contract source entry_point storage input syntax in
    Format.printf "%a\n" Ast_simplified.PP.expression output ;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ expression 2 $ expression 3 $ syntax) in
  let cmdname = "dry-run" in
  let docs = "Subcommand: run a smart-contract with the given storage and input." in
  (term , Term.info ~docs cmdname)

let run_function =
  let f source entry_point parameter syntax =
    toplevel @@
    let%bind output =
      Ligo.Run.run_function source entry_point parameter syntax in
    Format.printf "%a\n" Ast_simplified.PP.expression output ;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ expression 2 $ syntax) in
  let cmdname = "run-function" in
  let docs = "Subcommand: run a function with the given parameter." in
  (term , Term.info ~docs cmdname)

let evaluate_value =
  let f source entry_point syntax =
    toplevel @@
    let%bind output =
      Ligo.Run.evaluate_value source entry_point syntax in
    Format.printf "%a\n" Ast_simplified.PP.expression output ;
    ok ()
  in
  let term =
    Term.(const f $ source 0 $ entry_point 1 $ syntax) in
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
