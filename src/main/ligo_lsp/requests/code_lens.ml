open Handler
open Lsp_helpers
open Commands

(** Lenses that suggest adding "TZIP-16 compliance" attribute to potential
    storages.

    Workflow around this feature is the following:

    We use heuristics to guess which variables are likely some storages that
    follow TZIP-16, this happens in `on_doc` and populates the cache. For these
    storages we supply a code lens that adds a special LIGO attribute indicating
    TZIP-16 compliance, allowing for the next stage. This is done by executing
    [Lsp_commands.add_tzip16_attr] command.

    Then all variables annotated as TZIP-16 compliant are checked, and results
    go to diagnostics as normal warnings.

    We require such manual confirmation in regards to which storages to check
    because:
    1. Some of them may require no checking (e.g. used in tests), and running
       checks is relatively costly (requires JSON downloads at least once per
       coding session).
    2. Some storages may be non-TZIP-16 compliant intentionally (for tests).
    3. The used heuristics to discover TZIP-16-formatted storages are not 100%
       accurate.
     *)
let tzip16_compliance_lens (file : Path.t) : CodeLens.t list handler =
  with_cached_doc ~default:[] file
  @@ function
  | { document_version = None; _ } ->
    (* If document_version is not set, then the file is not opened, no need to
       display anything *)
    return []
  | { document_version = Some document_version; potential_tzip16_storages; _ } ->
    let@ normalize = ask_normalize in
    return
    @@ List.filter_map potential_tzip16_storages ~f:(fun potential_storage_var ->
           let%bind.Option var_range =
             match Ligo_prim.Value_var.get_location potential_storage_var with
             | Virtual _ -> None
             | File region -> Some (Range.of_region region)
           in
           let var_line = var_range.start.line in
           (* Range associated with a code lens must be a one-liner *)
           let range =
             if var_range.start.line = var_range.end_.line
             then var_range
             else Range.Construct.point var_line 0
           in
           let storage_var_position : Commands.storage_var_position =
             { line = var_line; file; document_version }
           in
           Option.return
           @@ CodeLens.create
                ~range
                ~command:
                  (Command.compile
                     ~command:(Ligo_lsp_commands.add_tzip16_attr ~normalize)
                     ~title:"Mark as TZIP-16 compatible storage"
                     ~arguments:storage_var_position)
                ())


(* This is usually called when file is opened and on every edit. *)
let on_code_lens (file : Path.t) : CodeLens.t list handler =
  let@ () = send_debug_msg "Code lenses called" in
  let@ tzip16_compliance_lens = tzip16_compliance_lens file in
  return tzip16_compliance_lens
