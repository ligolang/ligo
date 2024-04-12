open Handler
open Lsp_helpers

(** See {!Directive.extract_range_and_target}. Extracts a range and target to create a
    document link. *)
let extract_link_from_directive
    ~(normalize : Path.normalization)
    ~(relative_to_dir : Path.t)
    ~(mod_res : Preprocessor.ModRes.t option)
    : Preprocessor.Directive.t -> DocumentLink.t option
  =
 fun directive ->
  Option.map
    (Directive.extract_range_and_target ~normalize ~relative_to_dir ~mod_res directive)
    ~f:(fun (range, target) -> DocumentLink.create ~range ~target ())


(** Runs the handler for document link. This is usually called when file is opened and on
    every edit. For VSCode, the document link might run just after some delay when the
    last edit is made, so we provide the choice to the user to process the document on
    this request for better responsiveness. *)
let on_req_document_link (file : Path.t) : DocumentLink.t list option handler =
  let@ { diagnostics_pull_mode; _ } = ask_config in
  let@ () = send_debug_msg @@ "On document_link:" ^ Path.to_string file in
  let@ () =
    match diagnostics_pull_mode with
    | `OnDocumentLinkRequest ->
      let@ () =
        send_debug_msg @@ "document_link: processing doc " ^ Path.to_string file
      in
      process_doc file
    | `OnDocUpdate | `OnSave -> return ()
  in
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
  let@ normalize = ask_normalize in
  return
  @@ Option.map
       ~f:
         (List.filter_map
            ~f:
              (extract_link_from_directive
                 ~normalize
                 ~relative_to_dir:dir
                 ~mod_res:!mod_res))
  @@ directives_opt
