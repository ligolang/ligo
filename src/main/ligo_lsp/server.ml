(* We export the request implementations for testing them *)
module Requests = Requests
module Handler = Requests.Handler
open Lsp_helpers
open Handler

(** A sensible default configuration to use in case we have obtained no configuration from
    the LSP client. *)
let default_config : config =
  { max_number_of_problems = 100
  ; logging_verbosity = MessageType.Info
  ; disabled_features = []
  ; max_line_width = None
  ; completion_implementation = `With_scopes
  ; diagnostics_pull_mode = `OnDocUpdate
  ; metadata_checks_downloading = true
  ; metadata_checks_download_timeout_sec = 10.
  }


(** We support launching the language server with all capabilities, just semantic tokens,
    or no semantic tokens enabled, to provide better responsiveness for semantic tokens.
    If we ever migrate to OCaml version 5 or greater, we should handle this with multicore
    rather than launching multiple LSP servers. *)
type capability_mode =
  | Only_semantic_tokens (** Enable just semantic tokens. *)
  | All_capabilities (** Enable all capabilities. *)
  | No_semantic_tokens (** Enable all capabilities, except semantic tokens. *)

(* OCaml object methods are recursive by default, but OCaml doesn't play nicely with
   polymorphic recursion. The callers of this function have types that interact with GADTs
   and the solution I found was to extract this into a function outside the object. *)

(** If we crash, the language client (like VSCode) might spam messages notifying the user
    that a capability has failed. To prevent this, we provide this handler which will run
    the provided handler and prevent the LSP server from dying, and will display one crash
    message to the user (if the [bool ref] is [false]), which may be disabled if they
    press "Don't show again". The [bool ref] is changed to [true] if the user has clicked
    to not display the message again. The [default] value is used in case of a crash. *)
let try_with_crash_handler
    (type a)
    (has_caught_crash : bool ref)
    (action : a t)
    ~(default : a)
    : a t
  =
  let open Handler.Let_syntax in
  let log_crash_and_display_message (exn : exn) : unit t =
    let stack = Printexc.get_backtrace () in
    let%bind () =
      send_log_msg ~type_:Error
      @@ Format.asprintf "Unexpected exception: %a\n%s" Exn.pp exn stack
    in
    if !has_caught_crash
    then pass
    else (
      let don't_show_again = "Don't show again" in
      send_message_with_buttons
        ~type_:Error
        ~options:[ don't_show_again ]
        ~message:
          (Format.asprintf
             "An unexpected exception has occurred.\n\
              The language server will now operate at a limited capacity until the crash \
              has been worked around. Please report this as a bug together with an \
              example on how to reproduce this. A stack trace may be found in the logs \
              for LIGO Language Server. Details:\n\
              %a"
             Exn.pp
             exn)
        ~handler:(function
          | Error error -> send_log_msg ~type_:Error error.message
          | Ok None -> pass
          | Ok (Some { title }) ->
            if String.equal title don't_show_again then has_caught_crash := true;
            pass))
  in
  with_run_in_io
  @@ fun { unlift_io } ->
  try%lwt unlift_io action with
  | exn ->
    let%lwt () = unlift_io @@ log_crash_and_display_message exn in
    IO.return default


(** LIGO Language Server class.

    This is the main point of interaction beetween the code checking documents (parsing,
    typing, etc...), and the code of linol.

    The [server] class defines a method for each of the action that the lsp server
    receives, such as opening of a document, when a document changes, etc.. By default,
    the predefined method does nothing, so that users only need to override methods that
    they want the server to actually meaningfully interpret and respond to.

    The capability mode provided to this class is used in the boilerplate to initialize
    the language server in the language clients, allowing them to separately spawn a
    process for semantic tokens only and no semantic tokens, or both of them. See
    {!capability_mode}. *)
class lsp_server (capability_mode : capability_mode) =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super

    (** Used internally by linol to spawn queries. *)
    method spawn_query_handler = Linol_lwt.spawn

    (** A helper list with the default modes used by most request handlers to check
        whether they are enabled or not. *)
    val default_modes : capability_mode list = [ All_capabilities; No_semantic_tokens ]

    (** A table containing the buffers for opened documents. See [Docs_cache]. *)
    val get_scope_buffers : Docs_cache.t = Docs_cache.create ()

    (** The current language server configuration. Defaults to [default_config]. *)
    val mutable config : config = default_config

    (** The received LSP client capabilities. This will be filled during the initalize
        request. Defaults to [ClientCapabilities.create ()]. *)
    val mutable client_capabilities : ClientCapabilities.t = ClientCapabilities.create ()

    (** To have better accuracy in requests, there is a need to normalize file paths to
        remove indirections such as [.], [..], or symlinks. However, calling
        [Path.from_absolute] is expensive, and so this table caches file paths that were
        previously normalized to avoid excessive IO operations for normalization. *)
    val file_normalization_tbl = Hashtbl.create (module String)

    (** If [true], indicates that we reached a fatal [failwith] and the user has
        acknowledged it. [false] indicates that we haven't reached a crash yet, or the
        user has not yet acknowledged yet. *)
    val has_caught_crash : bool ref = ref false

    (** A sensible function to use for requests that use [~normalize]. This is also the
        function that's used by [ask_normalize]. See [file_normalization_tbl]. *)
    method private normalize (file : string) : Path.t =
      Hashtbl.find_or_add file_normalization_tbl file ~default:(fun () ->
          Path.from_absolute file)
    [@@alert "-from_absolute_performance"]

    (** Stores the download options for metadata checks, which will provide diagnostics
        for storages marked as TZIP-16-compliant. *)
    val mutable metadata_download_options : Tzip16_storage.download_options = `Unspecified

    (** Stores the path to the last ligo.json file, if found. *)
    val last_project_dir : Path.t option ref = ref None

    (** Caches the mod res used by some requests to resolve LIGO package imports. *)
    val mod_res : Preprocessor.ModRes.t option ref = ref None

    (** The [Handler.t] monad is used to run requests, but the server class operates in
        the [IO.t] monad. This method is used to run handlers. *)
    method run_handler : type a. notify_back_mockable -> a Handler.t -> a IO.t =
      fun notify_back handler ->
        Handler.run
          handler
          { notify_back
          ; config
          ; docs_cache = get_scope_buffers
          ; last_project_dir
          ; mod_res
          ; normalize = self#normalize
          ; metadata_download_options
          }

    (** Like [on_doc], but directly calls [Requests.on_doc] while handling crashes using
        [try_with_crash_handler]. *)
    method on_doc_with_handler ~process_immediately ?changes ~version file content =
      try_with_crash_handler has_caught_crash ~default:()
      @@ Requests.on_doc ~process_immediately ?changes ~version file content

    (** Processes the document cache, taking the [capability_mode] and
        [config.diagnostics_pull_mode] into consideration to avoid doing excessive work.
        This will use [on_doc_with_handler] to appropriately handle crashes. *)
    method on_doc
        : ?changes:TextDocumentContentChangeEvent.t list
          -> version:[ `New of Ligo_interface.document_version | `Unchanged ]
          -> Path.t
          -> string
          -> unit t =
      let process_immediately =
        match capability_mode with
        (* No need for calculating definitions/diagnostics here *)
        | Only_semantic_tokens -> false
        | All_capabilities | No_semantic_tokens ->
          (match config.diagnostics_pull_mode with
          | `OnDocUpdate -> true
          | `OnDocumentLinkRequest | `OnSave -> false)
      in
      self#on_doc_with_handler ~process_immediately

    (** Handles the notification for when a document is opened. *)
    method on_notif_doc_did_open ~notify_back document ~content : unit IO.t =
      let open Handler.Let_syntax in
      self#run_handler (Normal notify_back)
      @@
      let { diagnostics_pull_mode; _ } = config in
      let%bind normalize = ask_normalize in
      let file = DocumentUri.to_path ~normalize document.uri in
      match diagnostics_pull_mode, capability_mode with
      | _, Only_semantic_tokens | `OnDocumentLinkRequest, _ | `OnDocUpdate, _ ->
        self#on_doc file content ~version:(`New document.version)
      (* For [`OnSave] we should process doc that was opened *)
      | `OnSave, _ ->
        self#on_doc_with_handler
          ~process_immediately:true
          ~version:(`New document.version)
          file
          content

    (** Handles the notification for when a document is changed. *)
    method on_notif_doc_did_change
        ~notify_back
        document
        changes
        ~old_content:_old
        ~new_content
        : unit IO.t =
      let open Handler.Let_syntax in
      self#run_handler (Normal notify_back)
      @@ let%bind normalize = ask_normalize in
         let file = DocumentUri.to_path ~normalize document.uri in
         self#on_doc ~changes ~version:(`New document.version) file new_content

    (** Decodes the provided settings from JSON into [config] from ["ligoLanguageServer"]
        and applies the metadata download options. *)
    method decode_apply_settings
        (notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (settings : Yojson.Safe.t)
        : unit IO.t =
      let open Yojson.Safe.Util in
      (match to_option (member "ligoLanguageServer") settings with
      | None -> ()
      | Some ligo_language_server ->
        self#decode_and_store_ligo_language_server_config ligo_language_server);
      self#apply_metadata_options notify_back

    (** Decodes the provided settings from JSON into [config] from ["ligoLanguageServer"]. *)
    method decode_and_store_ligo_language_server_config
        (ligo_language_server : Yojson.Safe.t)
        : unit =
      let open Yojson.Safe.Util in
      config
        <- { max_number_of_problems =
               ligo_language_server
               |> member "maxNumberOfProblems"
               |> to_int_option
               |> Option.value ~default:default_config.max_number_of_problems
           ; logging_verbosity =
               (ligo_language_server
               |> member "loggingVerbosity"
               |> to_string_option
               |> function
               | Some "error" -> MessageType.Error
               | Some "warning" -> MessageType.Warning
               | Some "info" -> MessageType.Info
               | Some "log" -> MessageType.Log
               | Some _ | None -> default_config.logging_verbosity)
           ; disabled_features =
               ligo_language_server
               |> member "disabledFeatures"
               |> to_option to_list
               |> Option.value_map ~f:(List.map ~f:to_string) ~default:[]
           ; max_line_width =
               ligo_language_server |> member "maxLineWidth" |> to_int_option
           ; completion_implementation =
               (ligo_language_server
               |> member "completionImplementation"
               |> to_string_option
               |> function
               | Some "in-scope identifiers (can be slow)" -> `With_scopes
               | Some "all identifiers" -> `All_definitions
               | Some "only fields and keywords" -> `Only_keywords_and_fields
               | Some _ | None -> default_config.completion_implementation)
           ; diagnostics_pull_mode =
               (ligo_language_server
               |> member "diagnosticsPullMode"
               |> to_string_option
               |> function
               | Some "on doc update (can be slow)" -> `OnDocUpdate
               | Some "on document link request" -> `OnDocumentLinkRequest
               | Some "on save" -> `OnSave
               | Some _ | None -> default_config.diagnostics_pull_mode)
           ; metadata_checks_downloading =
               (ligo_language_server
               |> member "metadataChecksDownloading"
               |> function
               | `Bool v -> v
               | _ -> default_config.metadata_checks_downloading)
           ; metadata_checks_download_timeout_sec =
               (ligo_language_server
               |> member "metadataChecksDownloadTimeout"
               |> function
               | `Int v when v >= 0 -> float_of_int v
               | `Float v when Float.is_non_negative v -> v
               | _ -> default_config.metadata_checks_download_timeout_sec)
           }

    (** Applies the metadata download options. Warns the user if
        [config.diagnostics_pull_mode] is [`OnDocumentLinkRequest] and that request is
        disabled. *)
    method apply_metadata_options
        (notify_back : Linol_lwt.Jsonrpc2.notify_back)
        : unit IO.t =
      let open IO in
      let () =
        metadata_download_options
          <- Tzip16_storage.create_download_options
               ~enabled:config.metadata_checks_downloading
               ~timeout_sec:config.metadata_checks_download_timeout_sec
      in
      let* () =
        match config.diagnostics_pull_mode with
        | `OnDocumentLinkRequest
          when List.exists
                 ~f:(String.equal "textDocument/documentLink")
                 config.disabled_features ->
          notify_back#send_notification
          @@ ShowMessage
               (ShowMessageParams.create
                  ~message:
                    "Ligo Language Server: Diagnostics pull mode is 'on document link' \
                     while 'textDocument/documentLink' request is disabled. This can \
                     lead to missing diagnostics.\n\
                     Please enable 'textDocument/documentLink' request or change \
                     diagnostics pull mode."
                  ~type_:Warning)
        | _ -> IO.return ()
      in
      IO.return ()

    (** Handles the notification for when the language client requests the language server
        to initialize. Will decode and apply the configuration provided by the client and
        cache the supported client capabilities. *)
    method! on_req_initialize
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (init_params : InitializeParams.t)
        : InitializeResult.t IO.t =
      let open IO in
      (* Currently, the behaviors of these editors will be as follow:
         * Emacs: Will send [Some `Null]. Default config will be used.
           TODO: Fix this in #1706.
         * Vim: Will send [None], so we handle it similarly to Emacs.
           TODO: Fix this in #1707.
         * Visual Studio Code: As we support reading the configuration from
           this editor, we proceed with our ordinary business and decode it. *)
      let* () =
        match init_params.initializationOptions with
        | None -> self#apply_metadata_options notify_back
        | Some settings -> self#decode_apply_settings notify_back settings
      in
      client_capabilities <- init_params.capabilities;
      super#on_req_initialize ~notify_back init_params

    (** Checks whether the provided method name is a supported semantic tokens method
        ([true]) or not ([false]). Returns [false] if the provided string is not a
        semantic tokens method. *)
    method private is_supported_semantic_tokens : string -> bool =
      function
      | "textDocument/semanticTokens"
      | "textDocument/semanticTokens/range"
      | "textDocument/semanticTokens/full" -> true
      | "textDocument/semanticTokens/full/delta" | _ -> false

    (** Checks whether the provided method name refers to a method that is enabled
        ([true]) or not ([false]). *)
    method is_request_enabled (req : string) : bool =
      match self#is_supported_semantic_tokens req, capability_mode with
      | false, (All_capabilities | No_semantic_tokens)
      | true, (Only_semantic_tokens | All_capabilities) ->
        not @@ List.mem config.disabled_features ~equal:String.equal req
      | false, Only_semantic_tokens | true, No_semantic_tokens -> false

    (* TODO: When the document closes, we should thinking about removing the
       state associated to the file from the global hashtable state, to avoid
       leaking memory. We should also think about clearing diagnostics.
       Handle me with #1657. *)

    (** Handles the notification for when a document is closed. *)
    method on_notif_doc_did_close ~notify_back:_ _ : unit IO.t = Linol_lwt.return ()

    (** Declares support for hovers, if enabled. *)
    method! config_hover = Some (`Bool (self#is_request_enabled "textDocument/hover"))

    (** Declares support for formatting, if enabled. *)
    method config_formatting =
      Some (`Bool (self#is_request_enabled "textDocument/formatting"))

    (** Declares support for range formatting, if enabled. *)
    method config_range_formatting =
      Some (`Bool (self#is_request_enabled "textDocument/rangeFormatting"))

    (** Declares support for go to declaration, if enabled. *)
    method config_declaration =
      Some (`Bool (self#is_request_enabled "textDocument/declaration"))

    (** Declares support for go to definition, if enabled. *)
    method! config_definition =
      Some (`Bool (self#is_request_enabled "textDocument/definition"))

    (** Declares support for go to implementation, if enabled. *)
    method config_implementation =
      Some (`Bool (self#is_request_enabled "textDocument/implementation"))

    (** Declares support for document link, if enabled. *)
    method config_document_link_provider =
      if self#is_request_enabled "textDocument/documentLink"
      then Some (DocumentLinkOptions.create ())
      else None

    (** Declares support for document symbol, if enabled. *)
    method! config_symbol =
      Some (`Bool (self#is_request_enabled "textDocument/documentSymbol"))

    (** Declares support for rename and prepare rename, if enabled. *)
    method config_rename =
      if self#is_request_enabled "textDocument/rename"
      then (
        let rename_options =
          RenameOptions.create
            ~prepareProvider:(self#is_request_enabled "textDocument/prepareRename")
            ()
        in
        Some (`RenameOptions rename_options))
      else Some (`Bool false)

    (** Declares support for references, if enabled. *)
    method config_references =
      Some (`Bool (self#is_request_enabled "textDocument/references"))

    (** Declares support for type definition, if enabled. *)
    method config_type_definition =
      Some (`Bool (self#is_request_enabled "textDocument/typeDefinition"))

    (** Declares support for document highlight, if enabled. *)
    method config_highlight =
      Some (`Bool (self#is_request_enabled "textDocument/documentHighlight"))

    (** Declares support for folding range, if enabled. *)
    method config_folding_range =
      Some (`Bool (self#is_request_enabled "textDocument/foldingRange"))

    (** Declares support for completions, if enabled, and registers [.] and [@] as
        additional trigger characters. *)
    method! config_completion =
      if self#is_request_enabled "textDocument/completion"
      then (
        let completionItem =
          CompletionOptions.create_completionItem ~labelDetailsSupport:true ()
        in
        let completion_options =
          CompletionOptions.create ~completionItem ~triggerCharacters:[ "."; "@" ] ()
        in
        Some completion_options)
      else None

    (** Declares support for semantic tokens (full and range), if enabled, and registers
        supported token modifiers and token types. *)
    method config_semantic_tokens =
      if self#is_request_enabled "textDocument/semanticTokens"
      then (
        match capability_mode with
        | All_capabilities | Only_semantic_tokens ->
          let legend =
            SemanticTokensLegend.create
              ~tokenModifiers:
                (Array.to_list
                @@ Array.map ~f:Requests.mk_modifier_legend Requests.all_modifiers)
              ~tokenTypes:
                (Array.to_list @@ Array.map ~f:Requests.mk_type_legend Requests.all_types)
          in
          Some
            (`SemanticTokensOptions
              (SemanticTokensOptions.create
                 ~full:(`Full { delta = Some false })
                 ~legend
                 ~range:true
                 ()))
        | No_semantic_tokens -> None)
      else None

    (** Declares support for execute command, if enabled, and registers supported
        commands. *)
    method config_execute_command =
      (* Commands is an internally used thing, so handling all commands without
         looking at disabled features. However, we don't want "semantic tokens"
         LSP and "no semantic tokens" LSP to declare the same commands, it
         wouldn't work.
         *)
      let commands =
        match capability_mode with
        | All_capabilities | No_semantic_tokens ->
          Requests.Commands.Ligo_lsp_commands.
            [ (add_tzip16_attr ~normalize:self#normalize).id ]
        | Only_semantic_tokens -> []
      in
      Some (ExecuteCommandOptions.create ~commands ())

    (** Declares support for code lens, if enabled. *)
    method config_code_lens =
      Some
        (CodeLensOptions.create
           ~resolveProvider:(self#is_request_enabled "textDocument/codeLens")
           ())

    (** Declares support for inlay hints, if enabled. *)
    method config_inlay_hint =
      Some (`Bool (self#is_request_enabled "textDocument/inlayHint"))

    (** Provides the supported server capabilities as well as their configurations. *)
    method! config_modify_capabilities (c : ServerCapabilities.t) : ServerCapabilities.t =
      { c with
        hoverProvider = self#config_hover
      ; documentFormattingProvider = self#config_formatting
      ; documentSymbolProvider = self#config_symbol
      ; definitionProvider = self#config_definition
      ; declarationProvider = self#config_declaration
      ; implementationProvider = self#config_implementation
      ; inlayHintProvider = self#config_inlay_hint
      ; renameProvider = self#config_rename
      ; referencesProvider = self#config_references
      ; typeDefinitionProvider = self#config_type_definition
      ; documentHighlightProvider = self#config_highlight
      ; documentLinkProvider = self#config_document_link_provider
      ; foldingRangeProvider = self#config_folding_range
      ; documentRangeFormattingProvider = self#config_range_formatting
      ; completionProvider = self#config_completion
      ; semanticTokensProvider = self#config_semantic_tokens
      ; executeCommandProvider = self#config_execute_command
      ; codeLensProvider = self#config_code_lens
      }

    (** Handles a LSP notification from the client to the server. After the
        server is initialized, it will dynamically register file watchers for
        LIGO files as well as the project file. It will also dynamically
        register a watcher for configuration changes. *)
    method! on_notification
        : notify_back:(Server_notification.t -> unit IO.t)
          -> server_request:Linol_lwt.Jsonrpc2.send_request
          -> Client_notification.t
          -> unit IO.t =
      fun ~notify_back ~server_request ->
        let new_notify_back =
          new Linol_lwt.Jsonrpc2.notify_back
            ~notify_back
            ~server_request
            ~workDoneToken:None
            ~partialResultToken:None
            ()
        in
        let notif_run_handler action =
          (* Cannot eta-reduce, mutable `config` must be taken upon `action` run *)
          self#run_handler (Normal new_notify_back) @@ action
        in
        let ( let* ) = IO.( let* ) in
        function
        | Client_notification.Initialized ->
          (match client_capabilities.workspace with
          | None -> IO.return ()
          | Some { didChangeConfiguration; didChangeWatchedFiles; _ } ->
            let configuration_registration_opt =
              match didChangeConfiguration with
              | None -> None
              | Some { dynamicRegistration } ->
                (match dynamicRegistration with
                | None | Some false -> None
                | Some true ->
                  Some
                    (Registration.create
                       ~id:"ligoChangeConfiguration"
                       ~method_:"workspace/didChangeConfiguration"
                       ~registerOptions:`Null
                       ()))
            in
            let create_watcher_registration_opt patterns id =
              match didChangeWatchedFiles with
              | None -> None
              | Some { dynamicRegistration; _ } ->
                (match dynamicRegistration with
                | None | Some false -> None
                | Some true ->
                  let watchers =
                    List.map patterns ~f:(fun pattern ->
                        FileSystemWatcher.create ~globPattern:(`Pattern pattern) ())
                  in
                  let filewatcher =
                    DidChangeWatchedFilesRegistrationOptions.create ~watchers
                  in
                  Some
                    (Registration.create
                       ~id
                       ~method_:"workspace/didChangeWatchedFiles"
                       ~registerOptions:
                         (DidChangeWatchedFilesRegistrationOptions.yojson_of_t
                            filewatcher)
                       ()))
            in
            let registrations =
              List.filter_map
                ~f:Fn.id
                [ configuration_registration_opt
                ; create_watcher_registration_opt
                    [ Format.sprintf "**/%s" Project_root.ligoproject ]
                    "ligoJsonFileWatcher"
                ; create_watcher_registration_opt
                    Syntax.[ cameligo_glob; jsligo_glob ]
                    "ligoFileWatcher"
                ]
            in
            if List.is_empty registrations
            then IO.return ()
            else
              let* _req_id =
                new_notify_back#send_request
                  (Server_request.ClientRegisterCapability { registrations })
                  (function
                    | Error err ->
                      new_notify_back#send_log_msg ~type_:MessageType.Error err.message
                    | Ok () -> IO.return ())
              in
              IO.return ())
        | Client_notification.ChangeConfiguration _ ->
          (* FIXME: When we have a configuration change, we are getting `null` for some
             reason. Explicitly ask for the configuration as a workaround. *)
          (match client_capabilities.workspace with
          | None -> IO.return ()
          | Some { configuration; _ } ->
            (match configuration with
            | None | Some false -> IO.return ()
            | Some true ->
              let* () =
                new_notify_back#send_log_msg
                  ~type_:MessageType.Log
                  "Applying configuration change"
              in
              (* A soft restriction on number of files for which we can
                 simultaneously update diagnostics - prevents bursts of workload *)
              let max_files_for_diagnostics_update_at_once = 20 in
              let* _req_id =
                new_notify_back#send_request
                  (Server_request.WorkspaceConfiguration
                     { items =
                         [ ConfigurationItem.create ~section:"ligoLanguageServer" () ]
                     })
                  (function
                    | Error err ->
                      new_notify_back#send_log_msg ~type_:MessageType.Error err.message
                    | Ok configs ->
                      let* () =
                        if List.length configs = 1
                        then IO.return ()
                        else
                          new_notify_back#send_log_msg
                            ~type_:MessageType.Warning
                            (Format.asprintf
                               "Expected 1 workspace configuration, but got %d. \
                                Attempting to decode in order. Configurations received: \
                                %a"
                               (List.length configs)
                               (Fmt.Dump.list (fun ppf json ->
                                    Format.fprintf ppf "%s" (Yojson.Safe.to_string json)))
                               configs)
                      in
                      let* () =
                        Lwt_list.iter_p
                          (fun new_config ->
                            self#decode_and_store_ligo_language_server_config new_config;
                            self#apply_metadata_options new_notify_back)
                          configs
                      in
                      (* Update diagnostics *)
                      Lwt_list.iter_p
                        (fun ( path
                             , (Ligo_interface.{ code; _ } :
                                 Ligo_interface.unprepared_file_data) ) ->
                          notif_run_handler @@ self#on_doc path code ~version:`Unchanged)
                        (List.take
                           (Docs_cache.to_alist get_scope_buffers)
                           max_files_for_diagnostics_update_at_once))
              in
              IO.return ()))
        | Client_notification.DidChangeWatchedFiles { changes } ->
          let pp_file_change_type : FileChangeType.t Fmt.t =
           fun ppf ->
            Format.fprintf ppf
            <@ function
            | FileChangeType.Created -> "Created"
            | FileChangeType.Changed -> "Changed"
            | FileChangeType.Deleted -> "Deleted"
          in
          Fn.flip Lwt_list.iter_s changes
          @@ fun change ->
          let* () =
            new_notify_back#send_log_msg
              ~type_:MessageType.Log
              (Format.asprintf
                 "%a watched file: %a"
                 pp_file_change_type
                 change.type_
                 DocumentUri.pp
                 change.uri)
          in
          let file = DocumentUri.to_path ~normalize:self#normalize change.uri in
          let file_name = Filename.basename @@ Path.to_string file in
          if Filename.equal file_name Project_root.ligoproject
          then (
            last_project_dir := None;
            mod_res := None;
            IO.return ())
          else if Syntax.is_ligo file_name
          then
            (* If the file was changed from outside the editor, we clear the normalization
               table. *)
            Lwt.return (Hashtbl.clear file_normalization_tbl)
          else
            new_notify_back#send_log_msg
              ~type_:MessageType.Warning
              "Unknown watched file type"
        | Client_notification.DidSaveTextDocument { textDocument = { uri; _ }; _ } ->
          let { diagnostics_pull_mode; _ } = config in
          (match diagnostics_pull_mode, capability_mode with
          | _, Only_semantic_tokens | `OnDocUpdate, _ | `OnDocumentLinkRequest, _ ->
            IO.return ()
          | `OnSave, _ ->
            let path = DocumentUri.to_path ~normalize:self#normalize uri in
            let* () =
              new_notify_back#send_log_msg ~type_:MessageType.Log
              @@ Format.asprintf "Processing saved doc: %a" Path.pp path
            in
            let new_notify_back =
              Normal
                (new Linol_lwt.Jsonrpc2.notify_back
                   ~uri
                   ~notify_back
                   ~server_request
                   ~workDoneToken:None
                   ~partialResultToken:None
                   ())
            in
            self#run_handler new_notify_back
            @@ try_with_crash_handler has_caught_crash ~default:()
            @@ process_doc path)
        | notification -> super#on_notification ~notify_back ~server_request notification

    (** Handles a LSP request from the client to the server. This method will
        avoid running methods that have been disabled by the user, returning a
        default value (like [[]] or [None]). It will also ensure that we are
        working with the correct project root, recomputing the document and its
        caches in case it was changed. *)
    method! on_request
        : type r.  notify_back:(Server_notification.t -> unit IO.t)
                  -> server_request:Linol_lwt.Jsonrpc2.send_request
                  -> id:Req_id.t
                  -> r Client_request.t
                  -> r IO.t =
      fun ~notify_back ~server_request ~id (r : r Client_request.t) ->
        let open Handler.Let_syntax in
        let normalize = self#normalize in
        let run
            ?(allowed_modes : capability_mode list = default_modes)
            ~(uri : DocumentUri.t)
            ~(default : r)
            (handler : r Handler.t)
            : r IO.t
          =
          let method_ = (Client_request.to_jsonrpc_request r ~id).method_ in
          (* If the project root changed, let's repopulate the cache by deleting existing info and
             running [Requests.on_doc] again. *)
          let repopulate_cache : unit Handler.t =
            let file = DocumentUri.to_path ~normalize uri in
            match Docs_cache.find get_scope_buffers file with
            (* Shouldn't happen because [Requests.on_doc] should trigger and populate the
               cache. *)
            | None -> pass
            | Some { code; _ } ->
              let last_project_dir = !last_project_dir in
              if Option.equal
                   Path.equal
                   last_project_dir
                   (Project_root.get_project_root file)
              then pass
              else (
                let%bind () = Requests.drop_cached_definitions file in
                let%bind () =
                  send_log_msg
                    ~type_:MessageType.Log
                    "Project root changed: repopulating cache"
                in
                self#on_doc ?changes:None ~version:`Unchanged file code)
          in
          if self#is_request_enabled method_
             && List.mem ~equal:Caml.( = ) allowed_modes capability_mode
          then (
            let new_notify_back =
              Normal
                (new Linol_lwt.Jsonrpc2.notify_back
                   ~uri
                   ~notify_back
                   ~server_request
                   ~workDoneToken:None
                   ~partialResultToken:None
                   ())
            in
            self#run_handler new_notify_back
            @@ try_with_crash_handler has_caught_crash ~default
            @@ Handler.(repopulate_cache >>= fun () -> handler))
          else IO.return default
        in
        let run_command (handler : r Handler.t) : r IO.t =
          self#run_handler
            (Normal
               (new Linol_lwt.Jsonrpc2.notify_back
                  ~notify_back
                  ~server_request
                  ~workDoneToken:None
                  ~partialResultToken:None
                  ()))
            handler
        in
        match r with
        | Client_request.DocumentSymbol { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_document_symbol (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentFormatting { textDocument; options; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_formatting (DocumentUri.to_path ~normalize uri) options
        | Client_request.TextDocumentDeclaration { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_declaration position (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentDefinition { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_definition position (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentImplementation { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_implementation position (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentHover { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_hover position (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentPrepareRename { position; textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_prepare_rename position (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentRename { newName; position; textDocument; _ } ->
          let default = WorkspaceEdit.create () in
          let uri = textDocument.uri in
          run ~uri ~default
          @@ Requests.on_req_rename newName position (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentReferences { position; textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_references position (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentTypeDefinition { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_type_definition position (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentLink { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_document_link (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentHighlight { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_highlight position (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentFoldingRange { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_folding_range (DocumentUri.to_path ~normalize uri)
        | Client_request.TextDocumentRangeFormatting { range; textDocument; options; _ }
          ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_range_formatting
               (DocumentUri.to_path ~normalize uri)
               range
               options
        | Client_request.TextDocumentCompletion { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_completion position (DocumentUri.to_path ~normalize uri)
        | Client_request.UnknownRequest { meth = "DebugEcho"; _ } ->
          (* Used in tools/lsp-bench *)
          IO.return @@ `String "DebugEchoResponse"
        | Client_request.SemanticTokensFull { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~allowed_modes:[ All_capabilities; Only_semantic_tokens ] ~uri ~default:None
          @@ Requests.on_req_semantic_tokens_full (DocumentUri.to_path ~normalize uri)
        | Client_request.SemanticTokensRange { textDocument; range; _ } ->
          let uri = textDocument.uri in
          run ~allowed_modes:[ All_capabilities; Only_semantic_tokens ] ~uri ~default:None
          @@ Requests.on_req_semantic_tokens_range
               (DocumentUri.to_path ~normalize uri)
               range
        | Client_request.ExecuteCommand { command; arguments; _ } ->
          run_command @@ Requests.on_execute_command ~command ?arguments ()
        | Client_request.TextDocumentCodeLens { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~allowed_modes:[ All_capabilities; No_semantic_tokens ] ~uri ~default:[]
          @@ Requests.on_code_lens (DocumentUri.to_path ~normalize uri)
        | Client_request.InlayHint { textDocument; range; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_inlay_hint (DocumentUri.to_path ~normalize uri) range
        | _ -> super#on_request ~notify_back ~server_request ~id r
  end
