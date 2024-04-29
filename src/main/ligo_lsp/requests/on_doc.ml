open Handler
open Lsp_helpers

(** Creates a file containing [Project_root.default_project_file_contents] at the provided
    path and notifies the user about its creation. Updates the [Handler]'s
    [last_project_dir] and [mod_res] accordingly. *)
let create_default_project_file (project_root : Path.t) : unit Handler.t =
  let open Handler.Let_syntax in
  let%bind () =
    send_log_msg
      ~type_:Info
      (Format.asprintf "Creating project file at: %a." Path.pp project_root)
  in
  let uri = DocumentUri.of_path project_root in
  let file_edit_params =
    let edit =
      WorkspaceEdit.create
        ~documentChanges:
          [ `CreateFile (CreateFile.create ~uri ())
          ; `TextDocumentEdit
              (TextDocumentEdit.create
                 ~edits:
                   [ `TextEdit
                       (TextEdit.create
                          ~newText:Project_root.default_project_file_contents
                          ~range:(Range.Construct.point 0 0))
                   ]
                 ~textDocument:
                   (OptionalVersionedTextDocumentIdentifier.create ~uri ~version:1 ()))
          ]
        ()
    in
    ApplyWorkspaceEditParams.create ~edit ()
  in
  send_request (WorkspaceApplyEdit file_edit_params) (function
      | Error error ->
        let%bind () = send_log_msg ~type_:Error error.message in
        send_message ~type_:Error "Failed to create project file. Check logs."
      | Ok { applied; failureReason; failedChange } ->
        if applied
        then (
          let%bind () = update_project_root (Some project_root) in
          send_message
            ~type_:Info
            (Format.asprintf "Created project file at: %a." Path.pp project_root))
        else (
          let%bind () =
            Option.value_map ~default:pass ~f:(send_log_msg ~type_:Error) failureReason
          in
          let%bind () =
            Option.value_map
              ~default:pass
              ~f:(send_log_msg ~type_:Error <@ Format.sprintf "Failed change: %d")
              failedChange
          in
          send_message ~type_:Error "Failed to create project file. Check logs."))


(** Given the path to some contract, checks whether a project root for it exists, or asks
    the user to create one. Updates the [Handler]'s [last_project_dir] and [mod_res]
    accordingly.

    This function will try to detect legacy, deprecated project root files (.ligoproject
    or esy.json), workspace folders, and the provided file's parent directory in order to
    suggest directories to create the project file. *)
let detect_or_ask_to_create_project_file (file : Path.t) : unit Handler.t =
  let open Handler.Let_syntax in
  let%bind get_scope_buffers = ask_docs_cache in
  let project_root = Project_root.get_project_root file in
  let%bind () = update_project_root project_root in
  match Hashtbl.find get_scope_buffers file, project_root with
  | None, None ->
    send_request WorkspaceFolders (fun folders ->
        let%bind normalize = ask_normalize in
        let%bind folders =
          match folders with
          | Ok folders -> return folders
          | Error error ->
            let%bind () = send_log_msg ~type_:Error error.message in
            return []
        in
        let folders =
          List.map folders ~f:(fun { WorkspaceFolder.uri; _ } ->
              DocumentUri.to_path ~normalize uri)
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
                  Path.concat (normalize (common_prefix ^ title)) Project_root.ligoproject
                in
                create_default_project_file project_root)
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


(** Clears the cache for the provided file path. This is useful, for example, if the
    project root was changed. *)
let drop_cached_definitions : Path.t -> unit Handler.t =
 fun path ->
  let open Handler.Let_syntax in
  let%bind docs_cache = ask_docs_cache in
  when_some_ (Hashtbl.find docs_cache path)
  @@ fun { syntax
         ; code
         ; document_version
         ; definitions = _
         ; hierarchy = _
         ; potential_tzip16_storages = _
         ; parse_error_ranges = _
         ; lambda_types = _
         } ->
  set_docs_cache path (Ligo_interface.empty ~syntax ~code ~document_version)


(** We define here a helper that:
   - saves a new version of document to memory.
   - finds or asks the user to create a project file.
   Also, if [process_immediately = true], then it
   - processes a document.
   - stores the state resulting from the processing.
   - sends the diagnostics from the new state. *)
let on_doc
    :  process_immediately:bool -> ?changes:TextDocumentContentChangeEvent.t list
    -> version:[ `New of Ligo_interface.document_version | `Unchanged ] -> Path.t
    -> string -> unit Handler.t
  =
 fun ~process_immediately ?changes:_ ~version file code ->
  let open Handler.Let_syntax in
  let%bind syntax = get_syntax_exn file in
  let%bind () =
    send_debug_msg
    @@ Format.asprintf
         (if process_immediately
         then "caching and processing doc: %a"
         else "caching doc: %a")
         Path.pp
         file
  in
  let%bind () = detect_or_ask_to_create_project_file file in
  let%bind old_docs = ask_docs_cache in
  let document_version =
    match version with
    | `New v -> Some v
    | `Unchanged ->
      (match Hashtbl.find old_docs file with
      | None -> None
      | Some cache -> cache.document_version)
  in
  let%bind () =
    set_docs_cache file (Ligo_interface.empty ~syntax ~code ~document_version)
  in
  when_ process_immediately @@ process_doc file


(** This function checks if the given [Path.t] is cached, and if it's not, will read its
    contents from the disk and set the cache, setting [None] for definitions and hierarchy.
    If the file is cached, it will do nothing.

    This function is only for the case when the file is not opened in the editor. *)
let cache_doc_minimal : Path.t -> unit Handler.t =
 fun file ->
  let open Handler.Let_syntax in
  let%bind docs_cache = ask_docs_cache in
  if Hashtbl.mem docs_cache file
  then return ()
  else (
    let%bind () =
      send_debug_msg @@ Format.asprintf "Reading doc from disk (minimal): %a" Path.pp file
    in
    let%bind code =
      lift_io
      @@ Lwt_io.with_file ~mode:Input (Path.to_string file) (Lwt_io.read ?count:None)
    in
    let%bind syntax = get_syntax_exn file in
    set_docs_cache
      file
      (Ligo_interface.empty
         ~syntax
         ~code
         ~document_version:None
           (* This is valid because file is not opened in the editor *)))


(** A directed, possibly cyclic graph whose vertices are LIGO files and whose edges mean
    that a vertex [v1] [#import]s or [#include]s a vertex [v2]. *)
module File_graph = Lsp_helpers.Graph.Make (Path)

(** This function will scan the project root directory (if it exists) recursively and
    build a directed, possibly cyclic graph whose vertices are LIGO files and whose edges
    mean that a vertex [v1] [#import]s or [#include]s a vertex [v2]. *)
let build_file_graph : File_graph.t option Handler.t =
  let open Handler.Let_syntax in
  let%bind last_project_dir = ask_last_project_dir in
  match !last_project_dir with
  | None -> return None
  | Some project_root ->
    let%bind mod_res = ask_mod_res >>| fun res -> !res in
    let%bind normalize = ask_normalize in
    let files = Files.list_directory ~normalize ~include_library:true project_root in
    let%bind graph =
      fold_left files ~init:File_graph.empty ~f:(fun g file ->
          let%bind () = cache_doc_minimal file in
          let%bind directives =
            with_cst ~default:[] ~strict:false ~on_error:(const pass) file
            @@ function
            | CameLIGO cst -> return @@ Directive.extract_directives_cameligo cst
            | JsLIGO cst -> return @@ Directive.extract_directives_jsligo cst
          in
          (* Collect all links from [#include] and [#import] directives and remove as many
             indirections as possible, so duplicates won't exist in the file graph and
             cause problems. *)
          let deps =
            List.filter_map
              directives
              ~f:
                (Option.map ~f:snd
                <@ Directive.extract_range_and_target
                     ~normalize
                     ~relative_to_dir:(Path.dirname file)
                     ~mod_res)
          in
          return @@ File_graph.from_assocs [ file, deps ] g)
    in
    return @@ Some graph
