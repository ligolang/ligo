module Requests = Requests

(* TODO: use String, Option, Set, List & Hashtbl from Core *)
module LSet = Caml.Set.Make (Simple_utils.Location)
module List = Caml.List
module Option = Caml.Option

(* TODO: create Types /importmodule to do `open Types` instead *)
open Linol_lwt
open Linol_lwt.Jsonrpc2
open Lsp
open Requests.Handler

(* one env per document *)
let get_scope_buffers : (DocumentUri.t, Ligo_interface.file_data) Hashtbl.t =
  Hashtbl.create 32


let default_config : config =
  { max_number_of_problems = 100
  ; logging_verbosity = MessageType.Info
  ; disabled_features = []
  ; deprecated = false
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
    inherit server as super
    val mutable config : config = default_config
    val mutable client_capabilities : ClientCapabilities.t = ClientCapabilities.create ()

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back document ~content : unit IO.t =
      run_handler
        { notify_back = Normal notify_back; config; docs_cache = get_scope_buffers }
      @@ let@ { deprecated; _ } = ask_config in
         let@ () =
           if not deprecated
           then (
             match Utils.get_syntax document.uri with
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
         Requests.on_doc document.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a document is changed. *)
    method on_notif_doc_did_change
        ~notify_back
        document
        _changes
        ~old_content:_old
        ~new_content
        : unit IO.t =
      run_handler
        { notify_back = Normal notify_back; config; docs_cache = get_scope_buffers }
      @@ Requests.on_doc document.uri new_content

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
               |> Utils.value_map ~f:(List.map to_string) ~default:[]
           ; deprecated =
               ligo_language_server
               |> member "deprecated"
               |> to_bool_option
               |> Option.value ~default:default_config.deprecated
           }

    method! on_req_initialize
        ~(notify_back : notify_back)
        (init_params : InitializeParams.t)
        : InitializeResult.t IO.t =
      let* init_result = super#on_req_initialize ~notify_back init_params in
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
      IO.return init_result

    (* TODO: When the document closes, we should thinking about removing the
       state associated to the file from the global hashtable state, to avoid
       leaking memory. We should also think about clearing diagnostics.
       Handle me with #1657. *)
    method on_notif_doc_did_close ~notify_back:_ _ : unit IO.t = Linol_lwt.return ()
    method! config_hover = Some (`Bool true)
    method config_formatting = Some (`Bool true)
    method config_range_formatting = Some (`Bool true)
    method! config_definition = Some (`Bool true)
    method config_document_link_provider = Some (DocumentLinkOptions.create ())

    method config_rename =
      let rename_options = RenameOptions.create ?prepareProvider:(Some true) () in
      Some (`RenameOptions rename_options)

    method config_references = Some (`Bool true)
    method config_type_definition = Some (`Bool true)
    method config_folding_range = Some (`Bool true)

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
      }

    method! on_notification_unhandled
        : notify_back:notify_back -> Client_notification.t -> unit IO.t =
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
                    | Ok configs ->
                      IO.return (List.iter self#decode_apply_ligo_language_server configs))
              in
              IO.return ()))
        | n -> super#on_notification_unhandled ~notify_back n

    method! on_request
        : type r.  notify_back:(Server_notification.t -> unit IO.t)
                  -> server_request:send_request
                  -> id:Req_id.t
                  -> r Client_request.t
                  -> r IO.t =
      fun ~notify_back ~server_request ~id (r : _ Client_request.t) ->
        let run ~uri ~default =
          let method_ = (Client_request.to_jsonrpc_request r ~id).method_ in
          if List.mem method_ config.disabled_features
          then Fun.const @@ IO.return default
          else
            run_handler
              { notify_back =
                  Normal
                    (new notify_back
                       ~uri
                       ~notify_back
                       ~server_request
                       ~workDoneToken:None
                       ~partialResultToken:None
                       ())
              ; config
              ; docs_cache = get_scope_buffers
              }
        in
        match r with
        | Client_request.TextDocumentFormatting { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None @@ Requests.on_req_formatting uri
        | Client_request.TextDocumentDefinition { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None @@ Requests.on_req_definition position uri
        | Client_request.TextDocumentHover { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None @@ Requests.on_req_hover position uri
        | Client_request.TextDocumentPrepareRename { position; textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None @@ Requests.on_req_prepare_rename position uri
        | Client_request.TextDocumentRename { newName; position; textDocument; _ } ->
          let uri = textDocument.uri in
          let default = WorkspaceEdit.create () in
          run ~uri ~default @@ Requests.on_req_rename newName position uri
        | Client_request.TextDocumentReferences { position; textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None @@ Requests.on_req_references position uri
        | Client_request.TextDocumentTypeDefinition { textDocument; position; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None @@ Requests.on_req_type_definition position uri
        | Client_request.TextDocumentLink { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None @@ Requests.on_req_document_link uri
        | Client_request.TextDocumentFoldingRange { textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None @@ Requests.on_req_folding_range uri
        | Client_request.TextDocumentRangeFormatting { range; textDocument; _ } ->
          let uri = textDocument.uri in
          run ~uri ~default:None @@ Requests.on_req_range_formatting uri range
        | _ -> super#on_request ~notify_back ~server_request ~id r
  end
