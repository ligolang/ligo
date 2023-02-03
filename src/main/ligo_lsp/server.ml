module Requests = Requests

module Make (Ligo_api : Ligo_interface.LIGO_API) = struct
  (* TODO: use String, Option, Set, List & Hashtbl from Core *)
  module LSet = Caml.Set.Make (Simple_utils.Location)
  module List = Caml.List
  module Hashtbl = Caml.Hashtbl
  module Option = Caml.Option
  module String = Caml.String
  open Ligo_interface.Make (Ligo_api)
  open Utils

  (* open Types *)
  open Linol_lwt
  open Linol_lwt.Jsonrpc2
  open Lsp
  open Requests.Handler
  (* This file is free software, part of linol. See file "LICENSE" for more information *)

  (* Some user code

   The code here is just a placeholder to make this file compile, it is expected
   that users have an implementation of a processing function for input contents.

   Here we expect a few things:
   - a type to represent a state/environment that results from processing an
     input file
   - a function procdessing an input file (given the file contents as a string),
     which return a state/environment
   - a function to extract a list of diagnostics from a state/environment.
     Diagnostics includes all the warnings, errors and messages that the processing
     of a document are expected to be able to return.
*)

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

      (* one env per document *)
      val get_scope_buffers : (DocumentUri.t, get_scope_info) Hashtbl.t =
        Hashtbl.create 32

      (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
      method private _on_doc : DocumentUri.t -> string -> unit Handler.t =
        fun uri contents ->
          let new_state = Ligo_interface.unfold_get_scope @@ get_scope uri contents in
          Hashtbl.replace get_scope_buffers uri new_state;
          let@ () =
            send_debug_msg ("Updating DOC :" ^ DocumentUri.to_string uri)
          in
          let@ () =
            match new_state.errors with
            | [] -> send_debug_msg "No errors"
            | errors ->
              let value =
                List.map Main_errors.Formatter.error_json errors |> List.concat
              in
              send_debug_msg
                ("There are errors:\n *"
                ^ String.concat "\n *"
                @@ List.map (fun (x : Simple_utils.Error.t) -> x.content.message) value)
          in
          (* FIXME and add Warnings*)
          let extract_error_information : Main_errors.all -> (Range.t * string) list =
           fun e ->
            let errs = Main_errors.Formatter.error_json e in
            List.map
              (fun ({ content = { message; location; _ }; _ } : Simple_utils.Error.t) ->
                match location with
                | Some location ->
                  ( Option.value
                      ~default:Utils.dummy_range
                      (Utils.location_to_range location)
                  , message )
                | None -> Utils.dummy_range, message)
              errs
          in
          let diagnostics =
            new_state.errors
            |> List.map extract_error_information
            |> List.concat
            |> List.map (fun (range, message) -> Diagnostic.create ~message ~range ())
          in
          let@ () = send_diagnostic diagnostics in
          let@ () =
            match new_state.warnings with
            | [] -> send_debug_msg "No warnings"
            | _ -> send_debug_msg "There are warnings"
          in
          if new_state.has_info
          then send_debug_msg "There is result"
          else send_debug_msg "No result"

      (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
      method on_notif_doc_did_open ~notify_back d ~content : unit IO.t =
        run_handler
          { notify_back = Normal notify_back
          ; debug = debug_handlers
          ; docs_cache = get_scope_buffers
          }
        @@ self#_on_doc d.uri content

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
        @@ self#_on_doc d.uri new_content

      (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
      method on_notif_doc_did_close ~notify_back _ : unit IO.t =
        let* () = notify_back#send_log_msg ~type_:Info "Closed!!!" in
        Linol_lwt.return ()

      method on_req_hover_ : Position.t -> DocumentUri.t -> Hover.t option Handler.t =
        fun pos uri ->
          with_cached_doc uri None
          @@ fun get_scope_info ->
          when_some' (Requests.get_definition pos uri get_scope_info.definitions)
          @@ fun definition ->
          when_some' (get_syntax uri)
          @@ fun syntax ->
          let syntax_highlight = Syntax.to_string syntax in
          let hover_string = Requests.hover_string syntax definition in
          let marked_string : MarkedString.t =
            { value = Format.sprintf "```%s\n" syntax_highlight ^ hover_string ^ "\n```"
            ; language = None
            }
          in
          let contents = `MarkedString marked_string in
          let hover = Hover.create ~contents () in
          return (Some hover)

      method on_req_formatting_ : DocumentUri.t -> TextEdit.t list option Handler.t =
        fun uri ->
          let@ () =
            send_debug_msg @@ "Formatting request on " ^ DocumentUri.to_path uri
          in
          let formatted_text = formatting uri in
          match formatted_text with
          | Ok (result, _) ->
            let formatted_text =
              TextEdit.create ~newText:result ~range:whole_file_range
            in
            return (Some [ formatted_text ])
          | Error _ -> return None

      method on_req_definition_
          : Position.t -> DocumentUri.t -> Locations.t option Handler.t =
        fun pos uri ->
          with_cached_doc uri None
          @@ fun get_scope_info ->
          when_some' (Requests.get_definition pos uri get_scope_info.definitions)
          @@ fun definition ->
          let region = get_location definition in
          match region with
          | File region -> return @@ Some (`Location [ region_to_location region ])
          | Virtual _ -> return None

      method on_req_type_definition_
          : Position.t -> DocumentUri.t -> Locations.t option Handler.t =
        fun pos uri ->
          with_cached_doc uri None
          @@ fun get_scope_info ->
          when_some' (Requests.get_definition pos uri get_scope_info.definitions)
          @@ fun definition ->
          when_some'
            (match definition with
            | Variable vdef ->
              Option.bind (Requests.get_type vdef)
              @@ fun type_expression ->
              Option.some
              @@
              let location = type_expression.location in
              Option.value ~default:location
              @@ Option.bind (Utils.position_of_location location)
              @@ fun region ->
              Option.bind
                (Requests.get_definition region uri get_scope_info.definitions)
                (function
                  | Variable _vdef -> None
                  | Module _mdef -> None
                  | Type tdef -> Some tdef.range)
            | Type tdef -> Some tdef.range
            | Module _mdef -> None)
          @@ function
          | File region -> return @@ Some (`Location [ region_to_location region ])
          | Virtual _ -> return None

      method on_req_prepare_rename_
          : Position.t -> DocumentUri.t -> Range.t option Handler.t =
        fun pos uri ->
          with_cached_doc uri None
          @@ fun get_scope_info ->
          return @@ Requests.prepare_rename pos uri get_scope_info.definitions

      method on_req_rename_
          : string -> Position.t -> DocumentUri.t -> WorkspaceEdit.t Handler.t =
        fun new_name pos uri ->
          with_cached_doc uri (WorkspaceEdit.create ())
          @@ fun get_scope_info ->
          let@ value =
            when_some' (Requests.get_definition pos uri get_scope_info.definitions)
            @@ fun definition ->
            let references =
              Requests.get_all_references (get_location definition) get_scope_buffers
            in
            let changes =
              List.map
                (fun (file, ranges) ->
                  file, List.map (Requests.rename_reference new_name) ranges)
                references
            in
            return @@ Some (WorkspaceEdit.create ~changes ())
          in
          return @@ Option.value value ~default:(WorkspaceEdit.create ())

      method on_req_references_
          : Position.t -> DocumentUri.t -> Location.t list option Handler.t =
        fun pos uri ->
          with_cached_doc uri None
          @@ fun get_scope_info ->
          when_some (Requests.get_definition pos uri get_scope_info.definitions)
          @@ fun definition ->
          let references =
            Requests.get_all_references (get_location definition) get_scope_buffers
          in
          let@ () =
            send_debug_msg @@ "On references request on " ^ DocumentUri.to_path uri
          in
          let show_reference (uri, ranges) =
            DocumentUri.to_path uri
            ^ "\n"
            ^ String.concat "\n"
            @@ List.map Utils.range_to_string ranges
          in
          let@ () =
            send_debug_msg @@ String.concat "\n" @@ List.map show_reference references
          in
          return
          @@ List.flatten
          @@ List.map
               (fun (file, ranges) ->
                 List.map (fun range -> Location.create ~uri:file ~range) ranges)
               references

      method! config_hover = Some (`Bool true)
      method config_formatting = Some (`Bool true)
      method! config_definition = Some (`Bool true)

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
            run ~uri @@ self#on_req_formatting_ uri
          | Client_request.TextDocumentDefinition { textDocument; position; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ self#on_req_definition_ position uri
          | Client_request.TextDocumentHover { textDocument; position; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ self#on_req_hover_ position uri
          | Client_request.TextDocumentPrepareRename { position; textDocument; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ self#on_req_prepare_rename_ position uri
          | Client_request.TextDocumentRename { newName; position; textDocument; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ self#on_req_rename_ newName position uri
          | Client_request.TextDocumentReferences { position; textDocument; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ self#on_req_references_ position uri
          | Client_request.TextDocumentTypeDefinition { textDocument; position; _ } ->
            let uri = textDocument.uri in
            run ~uri @@ self#on_req_type_definition_ position uri
          | _ -> super#on_request ~notify_back ~id r
    end
end
