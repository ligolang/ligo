open Handler
open Lsp_helpers

let detect_or_ask_to_create_project_file (file : Path.t) : unit Handler.t =
  let@ get_scope_buffers = ask_docs_cache in
  let@ last_project_file = ask_last_project_file in
  let project_root = Project_root.get_project_root file in
  last_project_file := project_root;
  match Docs_cache.find get_scope_buffers file, project_root with
  | None, None ->
    send_request WorkspaceFolders (fun folders ->
        let@ folders =
          match folders with
          | Ok folders -> return folders
          | Error error ->
            let@ () = send_log_msg ~type_:Error error.message in
            return []
        in
        let folders =
          List.map folders ~f:(fun { WorkspaceFolder.uri; _ } -> DocumentUri.to_path uri)
        in
        let variants =
          [ ".ligoproject"; "esy.json" ] (* Top priority *)
          |> List.filter_map ~f:Path.(find_file_in_dir_and_parents @@ dirname file)
          |> List.map ~f:Path.dirname
          |> List.append folders (* Middle priority *)
          |> List.cons (Path.dirname file) (* Low priority *)
          (* Now we have an order we wanted, but reversed *)
          (* Deduplication and reverse without sorting *)
          |> List.fold ~init:[] ~f:(fun acc folder ->
                 if List.exists ~f:(Path.equal folder) acc then acc else folder :: acc)
          |> List.map ~f:(Fun.flip String.( ^ ) Filename.dir_sep <@ Path.to_string)
        in
        let open Simple_utils.List in
        match Ne.of_list_opt variants with
        | None -> send_message ~type_:Error "No variants to create `ligo.json`"
        | Some variants ->
          let common_prefix_length =
            let step = function
              | (_, false) as acc -> Fun.const acc
              | n, true ->
                fun x ->
                  if Ne.fold
                       (fun acc path ->
                         acc && String.length path > n && Char.equal (String.get path n) x)
                       true
                       variants
                  then n + 1, true
                  else n, false
            in
            Ne.hd variants |> String.fold ~init:(0, true) ~f:step |> fst
          in
          let common_prefix = String.prefix (Ne.hd variants) common_prefix_length in
          let variants =
            Ne.map
              (fun variant ->
                let variant = String.drop_prefix variant common_prefix_length in
                if String.is_empty variant then Filename.current_dir_name else variant)
              variants
          in
          let message =
            Format.sprintf
              "Can't find %s. Please pick a directory relative to \"%s\" to let LIGO \
               Language Server create it, or select \"No\" to ignore."
              Project_root.ligoproject
              common_prefix
          in
          let no = "No" in
          let options = Ne.to_list variants @ [ no ] in
          let handler
              :  (MessageActionItem.t option, Jsonrpc.Response.Error.t) result
              -> unit Handler.t
            = function
            | Error error -> send_log_msg ~type_:Error error.message
            | Ok None -> pass
            | Ok (Some { title }) ->
              if String.equal no title
              then pass
              else (
                let project_root =
                  Path.concat
                    (Path.from_absolute (common_prefix ^ title))
                    Project_root.ligoproject
                in
                let@ () =
                  send_log_msg
                    ~type_:Info
                    (Format.sprintf
                       "Creating project file at: %s."
                       (Path.to_string project_root))
                in
                let uri = DocumentUri.of_path project_root in
                let document_change = `CreateFile (CreateFile.create ~uri ()) in
                let edit = WorkspaceEdit.create ~documentChanges:[ document_change ] () in
                let w_e_params = ApplyWorkspaceEditParams.create ~edit () in
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
          send_message_with_buttons ~message ~options ~type_:Info ~handler)
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
