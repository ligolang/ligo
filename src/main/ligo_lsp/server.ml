(* We export the request implementations for testing them *)
module Requests = Requests
open Lsp_helpers
open Requests.Handler

(* one env per document *)
let get_scope_buffers : Docs_cache.t = Docs_cache.create ()

let default_config : config =
  { max_number_of_problems = 100
  ; logging_verbosity = MessageType.Info
  ; disabled_features = []
  ; max_line_width = None
  ; completion_implementation = `With_scopes
  }


type capability_mode =
  | Only_semantic_tokens
  | All_capabilities
  | No_semantic_tokens

let default_modes : capability_mode list = [ All_capabilities; No_semantic_tokens ]

(** Lsp server class

   This is the main point of interaction beetween the code checking documents
   (parsing, typing, etc...), and the code of linol.

   The [server] class defines a method for each of the action
   that the lsp server receives, such as opening of a document, when a document
   changes, etc.. By default, the method predefined does nothing (or errors out ?),
   so that users only need to override methods that they want the server to
   actually meaningfully interpret and respond to.
*)

class lsp_server (capability_mode : capability_mode) =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    method spawn_query_handler = Linol_lwt.spawn
    val mutable config : config = default_config
    val storage_invalidated : bool ref = ref true
    val mutable client_capabilities : ClientCapabilities.t = ClientCapabilities.create ()

    (** Stores the path to the last ligo.json file, if found. *)
    val last_project_file : Path.t option ref = ref None

    val mod_res : Preprocessor.ModRes.t option ref = ref None

    val on_doc =
      match capability_mode with
      | All_capabilities | No_semantic_tokens -> Requests.on_doc
      | Only_semantic_tokens -> Requests.on_doc_semantic_tokens

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back document ~content : unit IO.t =
      let file = DocumentUri.to_path document.uri in
      run_handler
        { notify_back = Normal notify_back
        ; config
        ; docs_cache = get_scope_buffers
        ; last_project_file
        ; mod_res
        }
      @@ on_doc file content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a document is changed. *)
    method on_notif_doc_did_change
        ~notify_back
        document
        changes
        ~old_content:_old
        ~new_content
        : unit IO.t =
      let file = DocumentUri.to_path document.uri in
      run_handler
        { notify_back = Normal notify_back
        ; config
        ; docs_cache = get_scope_buffers
        ; last_project_file
        ; mod_res
        }
      @@ on_doc ~changes file new_content

    method decode_apply_settings (settings : Yojson.Safe.t) : unit =
      let open Yojson.Safe.Util in
      match to_option (member "ligoLanguageServer") settings with
      | None -> ()
      | Some ligo_language_server ->
        self#decode_apply_ligo_language_server ligo_language_server

    method decode_apply_ligo_language_server (ligo_language_server : Yojson.Safe.t) : unit
        =
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
           }

    method! on_req_initialize
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (init_params : InitializeParams.t)
        : InitializeResult.t IO.t =
      (* Currently, the behaviors of these editors will be as follow:
         * Emacs: Will send [Some `Null]. Default config will be used.
           TODO: Fix this in #1706.
         * Vim: Will send [None], so we handle it similarly to Emacs.
           TODO: Fix this in #1707.
         * Visual Studio Code: As we support reading the configuration from
           this editor, we proceed with our ordinary business and decode it. *)
      let () =
        match init_params.initializationOptions with
        | None -> ()
        | Some settings -> self#decode_apply_settings settings
      in
      client_capabilities <- init_params.capabilities;
      super#on_req_initialize ~notify_back init_params

    method is_request_enabled : string -> bool =
      fun req -> not @@ List.mem config.disabled_features ~equal:String.equal req

    (* TODO: When the document closes, we should thinking about removing the
       state associated to the file from the global hashtable state, to avoid
       leaking memory. We should also think about clearing diagnostics.
       Handle me with #1657. *)
    method on_notif_doc_did_close ~notify_back:_ _ : unit IO.t = Linol_lwt.return ()
    method! config_hover = Some (`Bool (self#is_request_enabled "textDocument/hover"))

    method config_formatting =
      Some (`Bool (self#is_request_enabled "textDocument/formatting"))

    method config_range_formatting =
      Some (`Bool (self#is_request_enabled "textDocument/rangeFormatting"))

    method config_declaration =
      Some (`Bool (self#is_request_enabled "textDocument/declaration"))

    method! config_definition =
      Some (`Bool (self#is_request_enabled "textDocument/definition"))

    method config_implementation =
      Some (`Bool (self#is_request_enabled "textDocument/implementation"))

    method config_document_link_provider =
      if self#is_request_enabled "textDocument/documentLink"
      then Some (DocumentLinkOptions.create ())
      else None

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

    method config_references =
      Some (`Bool (self#is_request_enabled "textDocument/references"))

    method config_type_definition =
      Some (`Bool (self#is_request_enabled "textDocument/typeDefinition"))

    method config_folding_range =
      Some (`Bool (self#is_request_enabled "textDocument/foldingRange"))

    method! config_completion =
      let completionItem =
        CompletionOptions.create_completionItem ~labelDetailsSupport:true ()
      in
      let completion_options =
        CompletionOptions.create ~completionItem ~triggerCharacters:[ "."; "@" ] ()
      in
      Some completion_options

    method config_semantic_tokens =
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
      | No_semantic_tokens -> None

    method! config_modify_capabilities (c : ServerCapabilities.t) : ServerCapabilities.t =
      { c with
        hoverProvider = self#config_hover
      ; documentFormattingProvider = self#config_formatting
      ; definitionProvider = self#config_definition
      ; declarationProvider = self#config_declaration
      ; implementationProvider = self#config_implementation
      ; renameProvider = self#config_rename
      ; referencesProvider = self#config_references
      ; typeDefinitionProvider = self#config_type_definition
      ; documentLinkProvider = self#config_document_link_provider
      ; foldingRangeProvider = self#config_folding_range
      ; documentRangeFormattingProvider = self#config_range_formatting
      ; completionProvider = self#config_completion
      ; semanticTokensProvider = self#config_semantic_tokens
      }

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
            let watcher_registration_opt =
              match didChangeWatchedFiles with
              | None -> None
              | Some { dynamicRegistration; _ } ->
                (match dynamicRegistration with
                | None | Some false -> None
                | Some true ->
                  let pattern = Format.sprintf "**/%s" Project_root.ligoproject in
                  let ligo_project_watcher =
                    Lsp.Types.FileSystemWatcher.create ~globPattern:(`Pattern pattern) ()
                  in
                  let filewatcher =
                    Lsp.Types.DidChangeWatchedFilesRegistrationOptions.create
                      ~watchers:[ ligo_project_watcher ]
                  in
                  Some
                    (Registration.create
                       ~id:"ligoFileWatcher"
                       ~method_:"workspace/didChangeWatchedFiles"
                       ~registerOptions:
                         (Lsp.Types.DidChangeWatchedFilesRegistrationOptions.yojson_of_t
                            filewatcher)
                       ()))
            in
            let registrations =
              List.filter_map
                ~f:Fn.id
                [ configuration_registration_opt; watcher_registration_opt ]
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
              let* _req_id =
                new_notify_back#send_request
                  (Server_request.WorkspaceConfiguration
                     { items =
                         [ ConfigurationItem.create ~section:"ligoLanguageServer" () ]
                     })
                  (function
                    | Error err ->
                      new_notify_back#send_log_msg ~type_:MessageType.Error err.message
                    | Ok [ config ] ->
                      IO.return (self#decode_apply_ligo_language_server config)
                    | Ok configs ->
                      let* () =
                        new_notify_back#send_log_msg
                          ~type_:MessageType.Warning
                          (Format.asprintf
                             "Expected 1 workspace configuration, but got %d. Attempting \
                              to decode in order. Configurations received: %a"
                             (List.length configs)
                             (Fmt.Dump.list (fun ppf json ->
                                  Format.fprintf ppf "%s" (Yojson.Safe.to_string json)))
                             configs)
                      in
                      IO.return
                        (List.iter ~f:self#decode_apply_ligo_language_server configs))
              in
              IO.return ()))
        | Client_notification.DidChangeWatchedFiles { changes } ->
          let* () =
            new_notify_back#send_log_msg ~type_:MessageType.Log "DidChangeWatchedFiles"
          in
          if List.exists changes ~f:(fun { type_ = _; uri } ->
                 let path = Path.to_string (DocumentUri.to_path uri) in
                 Filename.(basename path = Project_root.ligoproject))
          then (
            last_project_file := None;
            mod_res := None);
          IO.return ()
        | notification -> super#on_notification ~notify_back ~server_request notification

    method! on_request
        : type r.  notify_back:(Server_notification.t -> unit IO.t)
                  -> server_request:Linol_lwt.Jsonrpc2.send_request
                  -> id:Req_id.t
                  -> r Client_request.t
                  -> r IO.t =
      fun ~notify_back ~server_request ~id (r : r Client_request.t) ->
        let run
            ?(allowed_modes : capability_mode list = default_modes)
            ~(uri : DocumentUri.t)
            ~(default : r)
            (handler : r Handler.t)
            : r IO.t
          =
          let method_ = (Client_request.to_jsonrpc_request r ~id).method_ in
          (* If the project root changed, let's repopulate the cache by running
             [Requests.on_doc] again. *)
          let repopulate_cache : unit Handler.t =
            let file = DocumentUri.to_path uri in
            match Docs_cache.find get_scope_buffers file with
            (* Shouldn't happen because [Requests.on_doc] should trigger and populate the
               cache. *)
            | None -> pass
            | Some { code; _ } ->
              let last_project_file = !last_project_file in
              if Option.equal
                   Path.equal
                   last_project_file
                   (Project_root.get_project_root file)
              then pass
              else on_doc ?changes:None file code
          in
          if self#is_request_enabled method_
             && List.mem ~equal:Caml.( = ) allowed_modes capability_mode
          then
            run_handler
              { notify_back =
                  Normal
                    (new Linol_lwt.Jsonrpc2.notify_back
                       ~uri
                       ~notify_back
                       ~server_request
                       ~workDoneToken:None
                       ~partialResultToken:None
                       ())
              ; config
              ; docs_cache = get_scope_buffers
              ; last_project_file
              ; mod_res
              }
              (bind repopulate_cache (fun () -> handler))
          else IO.return default
        in
        match r with
        | Client_request.TextDocumentFormatting { textDocument; options; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_formatting (DocumentUri.to_path uri) options
        | Client_request.TextDocumentDeclaration { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_declaration position (DocumentUri.to_path uri)
        | Client_request.TextDocumentDefinition { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_definition position (DocumentUri.to_path uri)
        | Client_request.TextDocumentImplementation { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_implementation position (DocumentUri.to_path uri)
        | Client_request.TextDocumentHover { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_hover position (DocumentUri.to_path uri)
        | Client_request.TextDocumentPrepareRename { position; textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_prepare_rename position (DocumentUri.to_path uri)
        | Client_request.TextDocumentRename { newName; position; textDocument; _ } ->
          let default = WorkspaceEdit.create () in
          let uri = textDocument.uri in
          run ~uri ~default
          @@ Requests.on_req_rename newName position (DocumentUri.to_path uri)
        | Client_request.TextDocumentReferences { position; textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_references position (DocumentUri.to_path uri)
        | Client_request.TextDocumentTypeDefinition { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_type_definition position (DocumentUri.to_path uri)
        | Client_request.TextDocumentLink { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_document_link (DocumentUri.to_path uri)
        | Client_request.TextDocumentFoldingRange { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_folding_range (DocumentUri.to_path uri)
        | Client_request.TextDocumentRangeFormatting { range; textDocument; options; _ }
          ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_range_formatting (DocumentUri.to_path uri) range options
        | Client_request.TextDocumentCompletion { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_completion position (DocumentUri.to_path uri)
        | Client_request.UnknownRequest { meth = "DebugEcho"; _ } ->
          (* Used in tools/lsp-bench *)
          IO.return @@ `String "DebugEchoResponse"
        | Client_request.SemanticTokensFull { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~allowed_modes:[ All_capabilities; Only_semantic_tokens ] ~uri ~default:None
          @@ Requests.on_req_semantic_tokens_full (DocumentUri.to_path uri)
        | Client_request.SemanticTokensRange { textDocument; range; _ } ->
          let uri = textDocument.uri in
          run ~allowed_modes:[ All_capabilities; Only_semantic_tokens ] ~uri ~default:None
          @@ Requests.on_req_semantic_tokens_range (DocumentUri.to_path uri) range
        | _ -> super#on_request ~notify_back ~server_request ~id r
  end
