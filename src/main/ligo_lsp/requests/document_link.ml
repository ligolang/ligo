open Handler
open Lsp_helpers

let extract_link_from_directive
    ~(relative_to_dir : Path.t)
    ~(mod_res : Preprocessor.ModRes.t option)
    : Preprocessor.Directive.t -> DocumentLink.t option
  =
 fun directive ->
  Option.map
    (Directive.extract_range_and_target ~relative_to_dir ~mod_res directive)
    ~f:(fun (range, target) -> DocumentLink.create ~range ~target ())


let on_req_document_link (file : Path.t) : DocumentLink.t list option handler =
  let@ () = send_debug_msg @@ "On document_link:" ^ Path.to_string file in
  let dir = Path.dirname file in
  let@ mod_res = ask_mod_res in
  let@ directives_opt =
    with_cst file ~default:None
    @@ fun cst ->
    return
    @@ Option.some
    @@ Dialect_cst.from_dialect
         { cameligo = Directive.extract_directives_cameligo
         ; jsligo = Directive.extract_directives_jsligo
         }
         cst
  in
  return
  @@ Option.map
       ~f:
         (List.filter_map
            ~f:(extract_link_from_directive ~relative_to_dir:dir ~mod_res:!mod_res))
  @@ directives_opt
