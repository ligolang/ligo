open Cmdliner
open Trace

let toplevel x =
  match x with
  | Trace.Ok ((), annotations) -> ignore annotations; ()
  | Errors ss ->
      Format.printf "Errors: %a\n%!" errors_pp @@ List.map (fun f -> f()) ss

let main =
  let term = Term.(const print_endline $ const "Ligo needs a command. Do ligo --help") in
  (term , Term.info "ligo")

let compile_file =
  let f source entry_point =
    toplevel @@
    let%bind contract =
      trace (simple_error "compile michelson") @@
      Ligo.Contract.compile_contract_file source entry_point in
    Format.printf "Contract:\n%s\n" contract ;
    ok ()
  in
  let term =
    let source =
      let open Arg in
      let info =
        let docv = "SOURCE_FILE" in
        let doc = "$(docv) is the path to the .ligo file of the contract." in
        info ~docv ~doc [] in
      required @@ pos 0 (some string) None info in
    let entry_point =
      let open Arg in
      let info =
        let docv = "ENTRY_POINT" in
        let doc = "$(docv) is entry-point that will be compiled." in
        info ~docv ~doc [] in
      value @@ pos 1 string "main" info in
    Term.(const f $ source $ entry_point) in
  let docs = "Compile contracts." in
  (term , Term.info ~docs "compile-contract")

let compile_expression =
  let f source entry_point expression =
    toplevel @@
    let%bind value =
      trace (simple_error "compile-input") @@
      Ligo.Contract.compile_contract_parameter source entry_point expression in
    Format.printf "Input:\n%s\n" value;
    ok ()
  in
  let term =
    let source =
      let open Arg in
      let docv = "SOURCE_FILE" in
      let doc = "$(docv) is the path to the .ligo file of the contract." in
      let info = info ~docv ~doc [] in
      required @@ pos 0 (some string) None info in
    let entry_point =
      let open Arg in
      let docv = "ENTRY_POINT" in
      let doc = "$(docv) is the entry-point of the contract." in
      let info = info ~docv ~doc [] in
      required @@ pos 1 (some string) None info in
    let expression =
      let open Arg in
      let docv = "EXPRESSION" in
      let doc = "$(docv) is the expression that will be compiled." in
      let info = info ~docv ~doc [] in
      required @@ pos 2 (some string) None info in
    Term.(const f $ source $ entry_point $ expression) in
  let docs = "Compile contracts parameters." in
  (term , Term.info ~docs "compile-parameter")

let () = Term.exit @@ Term.eval_choice main [compile_file ; compile_expression]
