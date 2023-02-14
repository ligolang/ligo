module Requests = Requests

module Make (Ligo_api : Ligo_interface.LIGO_API) = struct
  (* TODO: use String, Option, Set, List & Hashtbl from Core *)
  module LSet = Caml.Set.Make (Simple_utils.Location)
  module List = Caml.List
  module Option = Caml.Option
  open Ligo_interface.Make (Ligo_api)

  (* open Types *)
  open Linol_lwt
  open Linol_lwt.Jsonrpc2
  open Lsp
  module Requests = Requests.Make (Ligo_api)
  open Requests.Handler
  (* This file is free software, part of linol. See file "LICENSE" for more information *)

  (* one env per document *)
  let get_scope_buffers : (DocumentUri.t, Ligo_interface.file_data) Hashtbl.t =
    Hashtbl.create 32


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

      (* FIXME we should read this from VSCode config *)
      val debug_handlers = false

      (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
      method on_notif_doc_did_open ~notify_back d ~content : unit IO.t =
        run_handler
          { notify_back = Normal notify_back
          ; debug = debug_handlers
          ; docs_cache = get_scope_buffers
          }
        @@ Requests.on_doc d.uri content

      (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
      method on_notif_doc_did_change
          ~notify_back
          d
          _c
          ~old_content:_old
          ~new_content
          : unit IO.t =
        run_handler
          { notify_back = Normal notify_back
          ; debug = debug_handlers
          ; docs_cache = get_scope_buffers
          }
        @@ Requests.on_doc d.uri new_content

      (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
      method on_notif_doc_did_close ~notify_back _ : unit IO.t =
        let* () = notify_back#send_log_msg ~type_:Info "Closed!!!" in
        Linol_lwt.return ()

      method! config_hover = Some (`Bool true)
      method config_formatting = Some (`Bool true)
      method! config_definition = Some (`Bool true)
      method config_document_link_provider = Some (DocumentLinkOptions.create ())

      method config_rename =
        let rename_options = RenameOptions.create ?prepareProvider:(Some true) () in
        Some (`RenameOptions rename_options)

      method config_references = Some (`Bool true)
      method config_type_definition = Some (`Bool true)

      method! config_modify_capabilities (c : ServerCapabilities.t) : ServerCapabilities.t
          =
        { c with
          hoverProvider = self#config_hover
        ; documentFormattingProvider = self#config_formatting
        ; definitionProvider = self#config_definition
        ; renameProvider = self#config_rename
        ; referencesProvider = self#config_references
        ; typeDefinitionProvider = self#config_type_definition
        ; documentLinkProvider = self#config_document_link_provider
        }

      method! on_request
          : type r.  notify_back:(Server_notification.t -> unit Lwt.t)
                    -> id:Req_id.t
                    -> r Client_request.t
                    -> r IO.t =
        fun ~notify_back ~id (r : _ Client_request.t) ->
          let run ~uri =
            run_handler
              { notify_back = Normal (new notify_back ~uri ~notify_back ())
              ; debug = debug_handlers
              ; docs_cache = get_scope_buffers
              }
          in
          match r with
          | Client_request.TextDocumentFormatting { textDocument; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ Requests.on_req_formatting uri
          | Client_request.TextDocumentDefinition { textDocument; position; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ Requests.on_req_definition position uri
          | Client_request.TextDocumentHover { textDocument; position; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ Requests.on_req_hover position uri
          | Client_request.TextDocumentPrepareRename { position; textDocument; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ Requests.on_req_prepare_rename position uri
          | Client_request.TextDocumentRename { newName; position; textDocument; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ Requests.on_req_rename newName position uri
          | Client_request.TextDocumentReferences { position; textDocument; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ Requests.on_req_references position uri
          | Client_request.TextDocumentTypeDefinition { textDocument; position; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ Requests.on_req_type_definition position uri
          | Client_request.TextDocumentLink { textDocument; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ Requests.on_req_document_link uri
          | _ -> super#on_request ~notify_back ~id r
    end
end
