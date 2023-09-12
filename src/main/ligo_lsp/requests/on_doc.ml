open Handler
open Lsp_helpers

(* We define here a helper that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
let on_doc
    : ?changes:TextDocumentContentChangeEvent.t list -> Path.t -> string -> unit Handler.t
  =
 fun ?changes:_ file contents ->
  let open Ligo_interface in
  let@ () = send_debug_msg @@ "Updating DOC :" ^ Path.to_string file in
  let@ get_scope_buffers = ask_docs_cache in
  let@ syntax =
    match Path.get_syntax file with
    | None ->
      lift_IO @@ failwith @@ "Expected file with LIGO code, got: " ^ Path.to_string file
    | Some s -> return s
  in
  let@ { config = { deprecated; max_number_of_problems; _ }; _ } = ask in
  (* We want the following behavior:
     * no changes (document opened): download JSON, update storage diagnostics
     * storage not found: do not download JSON, clear storage diagnostics
     * storage not changed: do not download JSON, keep previous storage diagnostics
     * storage changed: download JSON, update storage diagnostics *)
  let@ ({ definitions; _ } as defs_and_diagnostics : defs_and_diagnostics) =
    lift_IO
    @@ Ligo_interface.get_defs_and_diagnostics
         ?json_download:None
         ~deprecated
         ~code:contents
         file
  in
  Docs_cache.set
    get_scope_buffers
    ~key:file
    ~data:{ definitions; syntax; code = contents };
  let deprecation_warnings =
    match syntax with
    | PascaLIGO ->
      [ Diagnostics.
          { message = "PascaLIGO is not officially supported in this LIGO version"
          ; severity = DiagnosticSeverity.Warning
          ; location = { range = Range.dummy; path = file }
          }
      ]
    | CameLIGO | JsLIGO -> []
  in
  let diags_by_file =
    let simple_diags = Diagnostics.get_diagnostics defs_and_diagnostics in
    Diagnostics.partition_simple_diagnostics
      file
      (Some max_number_of_problems)
      (simple_diags @ deprecation_warnings)
  in
  (* Corner case: clear diagnostics for this file in case there are none. *)
  let@ () =
    let uri = DocumentUri.of_path file in
    if List.Assoc.mem diags_by_file ~equal:DocumentUri.equal uri
    then pass
    else send_diagnostic uri []
  in
  iter diags_by_file ~f:(Simple_utils.Utils.uncurry send_diagnostic)
