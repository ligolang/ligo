open Handler
open Lsp_helpers

let detect_or_ask_to_create_project_file (file : Path.t) : unit Handler.t =
  let@ get_scope_buffers = ask_docs_cache in
  let@ last_project_file = ask_last_project_file in
  let project_root = Project_root.get_project_root file in
  last_project_file := project_root;
  match Docs_cache.find get_scope_buffers file, project_root with
  | None, None ->
    let variants =
      [ ".ligoproject"; "esy.json" ]
      |> List.filter_map ~f:Path.(find_file_in_dir_and_parents @@ dirname file)
      |> List.cons file
      |> List.map ~f:Path.dirname
      |> List.dedup_and_sort ~compare:Path.compare
      |> List.map ~f:Path.to_string
      |> List.mapi ~f:(fun i dir ->
             let i = Int.to_string (i + 1) in
             Format.sprintf "%s: Create %s at %s." i Project_root.ligoproject dir, i, dir)
    in
    let no = "No" in
    let messages, options, _ = List.unzip3 variants in
    let options = no :: options in
    let message =
      Format.sprintf
        "Can't find %s. Do you want to create it? %s"
        Project_root.ligoproject
        (String.concat ~sep:"\n" messages)
    in
    let handler
        :  (Lsp.Types.MessageActionItem.t option, Jsonrpc.Response.Error.t) result
        -> unit Handler.t
      = function
      | Error error -> send_log_msg ~type_:Error error.message
      | Ok None -> pass
      | Ok (Some { title }) ->
        if String.equal no title
        then pass
        else (
          match
            List.find variants ~f:(fun (_, option, _) -> String.equal title option)
          with
          | None ->
            send_log_msg ~type_:Error (Format.sprintf "Invalid button title: %s" title)
          | Some (_, _, dir) ->
            let project_root =
              Path.concat (Path.from_absolute dir) Project_root.ligoproject
            in
            let@ () =
              send_log_msg
                ~type_:Info
                (Format.sprintf
                   "Creating project file at: %s."
                   (Path.to_string project_root))
            in
            let uri = DocumentUri.of_path project_root in
            let document_change = `CreateFile (Lsp.Types.CreateFile.create ~uri ()) in
            let edit = WorkspaceEdit.create ~documentChanges:[ document_change ] () in
            let w_e_params = Lsp.Types.ApplyWorkspaceEditParams.create ~edit () in
            send_request (WorkspaceApplyEdit w_e_params) (function
                | Error error -> send_log_msg ~type_:Error error.message
                | Ok { applied; failureReason; failedChange = _ } ->
                  if applied
                  then
                    let@ last_project_file = ask_last_project_file in
                    let () = last_project_file := Some project_root in
                    send_message
                      ~type_:Info
                      (Format.sprintf
                         "Created project file at: %s."
                         (Path.to_string project_root))
                  else (
                    let msg =
                      Option.value_map
                        ~default:""
                        ~f:(Format.sprintf " %s.")
                        failureReason
                    in
                    send_message
                      ~type_:Error
                      (Format.sprintf "Failed to create project file.%s" msg))))
    in
    send_message_with_buttons ~message ~options ~type_:Info ~handler
  | None, Some project_root ->
    send_log_msg
      ~type_:Info
      (Format.sprintf
         "Found %s at %s."
         Project_root.ligoproject
         (Path.to_string project_root))
  | Some _, (None | Some _) -> pass


(** We define here a helper that will:
    - process a document
    - store the state resulting from the processing
    - return the diagnostics from the new state
    - find or ask the user to create a project file *)
let on_doc
    : ?changes:TextDocumentContentChangeEvent.t list -> Path.t -> string -> unit Handler.t
  =
 fun ?changes:_ file contents ->
  let@ () = send_debug_msg @@ "Updating doc: " ^ Path.to_string file in
  let@ { config = { max_number_of_problems; _ }; docs_cache; last_project_file; _ } =
    ask
  in
  let@ syntax =
    match Path.get_syntax file with
    | None ->
      lift_IO @@ failwith @@ "Expected file with LIGO code, got: " ^ Path.to_string file
    | Some s -> return s
  in
  let@ () = detect_or_ask_to_create_project_file file in
  (* We want the following behavior:
     * no changes (document opened): download JSON, update storage diagnostics
     * storage not found: do not download JSON, clear storage diagnostics
     * storage not changed: do not download JSON, keep previous storage diagnostics
     * storage changed: download JSON, update storage diagnostics *)
  let@ ({ definitions; _ } as defs_and_diagnostics) =
    lift_IO
    @@ Ligo_interface.get_defs_and_diagnostics
         ~project_root:!last_project_file
         ?json_download:None
         ~code:contents
         file
  in
  Docs_cache.set docs_cache ~key:file ~data:{ definitions; syntax; code = contents };
  let diags_by_file =
    let simple_diags = Diagnostics.get_diagnostics defs_and_diagnostics in
    Diagnostics.partition_simple_diagnostics
      file
      (Some max_number_of_problems)
      simple_diags
  in
  (* Corner case: clear diagnostics for this file in case there are none. *)
  let@ () =
    let uri = DocumentUri.of_path file in
    if List.Assoc.mem diags_by_file ~equal:DocumentUri.equal uri
    then pass
    else send_diagnostic uri []
  in
  iter diags_by_file ~f:(Simple_utils.Utils.uncurry send_diagnostic)
