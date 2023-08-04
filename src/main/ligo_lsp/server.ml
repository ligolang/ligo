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
  ; deprecated = false
  ; max_line_width = None
  }


(* Lsp server class

   This is the main point of interaction beetween the code checking documents
   (parsing, typing, etc...), and the code of linol.

   The [server] class defines a method for each of the action
   that the lsp server receives, such as opening of a document, when a document
   changes, etc.. By default, the method predefined does nothing (or errors out ?),
   so that users only need to override methods that they want the server to
   actually meaningfully interpret and respond to.
*)

class lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    method spawn_query_handler = Linol_lwt.spawn
    val mutable config : config = default_config
    val mutable client_capabilities : ClientCapabilities.t = ClientCapabilities.create ()

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back document ~content : unit IO.t =
      let file = DocumentUri.to_path document.uri in
      run_handler
        { notify_back = Normal (document.uri, notify_back)
        ; config
        ; docs_cache = get_scope_buffers
        }
      @@ let@ { deprecated; _ } = ask_config in
         let@ () =
           if not deprecated
           then (
             match Path.get_syntax file with
             | None -> return ()
             | Some PascaLIGO ->
               send_message
                 ~type_:MessageType.Error
                 "PascaLIGO is deprecated and not fully supported. To use the language \
                  server with it, enable \"ligoLanguageServer.deprecated\" in the Visual \
                  Studio Code configuration."
             | Some (CameLIGO | JsLIGO) -> return ())
           else return ()
         in
         Requests.on_doc file content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a document is changed. *)
    method on_notif_doc_did_change
        ~notify_back
        document
        _changes
        ~old_content:_old
        ~new_content
        : unit IO.t =
      let file = DocumentUri.to_path document.uri in
      run_handler
        { notify_back = Normal (document.uri, notify_back)
        ; config
        ; docs_cache = get_scope_buffers
        }
      @@ Requests.on_doc file new_content

    method decode_apply_settings (settings : Yojson.Safe.t) : unit =
      let open Yojson.Safe.Util in
      match to_option (member "ligoLanguageServer") settings with
      | None -> config <- { config with deprecated = true }
      | Some ligo_language_server ->
        self#decode_apply_ligo_language_server ligo_language_server

    method decode_apply_ligo_language_server (ligo_language_server : Yojson.Safe.t) : unit
        =
      let open Yojson.Safe.Util in
      config
        <- { config with
             max_number_of_problems =
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
           ; deprecated =
               ligo_language_server
               |> member "deprecated"
               |> to_bool_option
               |> Option.value ~default:default_config.deprecated
           ; max_line_width =
               ligo_language_server |> member "maxLineWidth" |> to_int_option
           }

    method! on_req_initialize
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (init_params : InitializeParams.t)
        : InitializeResult.t IO.t =
      (* Currently, the behaviors of these editors will be as follow:
         * Emacs: Will send [Some `Null]. In [decode_apply_settings] we handle
           this by seeting [deprecated] as [true] so this editor may launch
           when opening PascaLIGO files.
           TODO: Fix this in #1706.
         * Vim: Will send [None], so we handle it similarly to Emacs.
           TODO: Fix this in #1707.
         * Visual Studio Code: As we support reading the configuration from
           this editor, we proceed with our ordinary business and decode it. *)
      let () =
        match init_params.initializationOptions with
        | None -> config <- { config with deprecated = true }
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

    method! config_definition =
      Some (`Bool (self#is_request_enabled "textDocument/definition"))

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

    method! config_modify_capabilities (c : ServerCapabilities.t) : ServerCapabilities.t =
      { c with
        hoverProvider = self#config_hover
      ; documentFormattingProvider = self#config_formatting
      ; definitionProvider = self#config_definition
      ; renameProvider = self#config_rename
      ; referencesProvider = self#config_references
      ; typeDefinitionProvider = self#config_type_definition
      ; documentLinkProvider = self#config_document_link_provider
      ; foldingRangeProvider = self#config_folding_range
      ; documentRangeFormattingProvider = self#config_range_formatting
      ; completionProvider = self#config_completion
      }

    method! on_notification_unhandled
        : notify_back:Linol_lwt.Jsonrpc2.notify_back -> Client_notification.t -> unit IO.t
        =
      let open IO in
      fun ~notify_back -> function
        | Client_notification.Initialized ->
          (match client_capabilities.workspace with
          | None -> IO.return ()
          | Some { didChangeConfiguration; _ } ->
            (match didChangeConfiguration with
            | None -> IO.return ()
            | Some { dynamicRegistration } ->
              (match dynamicRegistration with
              | None | Some false -> IO.return ()
              | Some true ->
                let register_change_configuration : Registration.t =
                  { id = "ligoChangeConfiguration"
                  ; method_ = "workspace/didChangeConfiguration"
                  ; registerOptions = Some `Null
                  }
                in
                let* _req_id =
                  notify_back#send_request
                    (Server_request.ClientRegisterCapability
                       { registrations = [ register_change_configuration ] })
                    (function
                      | Error err ->
                        notify_back#send_log_msg ~type_:MessageType.Error err.message
                      | Ok () -> IO.return ())
                in
                IO.return ())))
        | Client_notification.ChangeConfiguration _ ->
          (* FIXME: When we have a configuration change, we are getting `null`
             for some reason. Explicitly ask for the configuration as a
             workaround. *)
          (match client_capabilities.workspace with
          | None -> IO.return ()
          | Some { configuration; _ } ->
            (match configuration with
            | None | Some false -> IO.return ()
            | Some true ->
              let* _req_id =
                notify_back#send_request
                  (Server_request.WorkspaceConfiguration
                     { items =
                         [ ConfigurationItem.create ~section:"ligoLanguageServer" () ]
                     })
                  (function
                    | Error err ->
                      notify_back#send_log_msg ~type_:MessageType.Error err.message
                    | Ok [ config ] ->
                      IO.return (self#decode_apply_ligo_language_server config)
                    | Ok configs ->
                      let* () =
                        notify_back#send_log_msg
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
        | n -> super#on_notification_unhandled ~notify_back n

    method! on_request
        : type r.  notify_back:(Server_notification.t -> unit IO.t)
                  -> server_request:Linol_lwt.Jsonrpc2.send_request
                  -> id:Req_id.t
                  -> r Client_request.t
                  -> r IO.t =
      fun ~notify_back ~server_request ~id (r : _ Client_request.t) ->
        let run ~uri ~default =
          let method_ = (Client_request.to_jsonrpc_request r ~id).method_ in
          if self#is_request_enabled method_
          then
            run_handler
              { notify_back =
                  Normal
                    ( uri
                    , new Linol_lwt.Jsonrpc2.notify_back
                        ~uri
                        ~notify_back
                        ~server_request
                        ~workDoneToken:None
                        ~partialResultToken:None
                        () )
              ; config
              ; docs_cache = get_scope_buffers
              }
          else Fun.const @@ IO.return default
        in
        match r with
        | Client_request.TextDocumentFormatting { textDocument; options; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_formatting (DocumentUri.to_path uri) options
        | Client_request.TextDocumentDefinition { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_definition position (DocumentUri.to_path uri)
        | Client_request.TextDocumentHover { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_hover position (DocumentUri.to_path uri)
        | Client_request.TextDocumentPrepareRename { position; textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None
          @@ Requests.on_req_prepare_rename position (DocumentUri.to_path uri)
        | Client_request.TextDocumentRename { newName; position; textDocument; _ } ->
          let uri = textDocument.uri in
          let default = WorkspaceEdit.create () in
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
        | _ -> super#on_request ~notify_back ~server_request ~id r
  end
