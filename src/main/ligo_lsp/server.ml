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
  open Utils.Maybe

  (* open Types *)
  open Linol_lwt
  open Linol_lwt.Jsonrpc2
  open Lsp

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

  let with_get_scope_info
      : type r.
        (DocumentUri.t, get_scope_info) Hashtbl.t
        -> DocumentUri.t
        -> r
        -> (get_scope_info -> r IO.t)
        -> r IO.t
    =
   fun get_scope_buffers uri default f ->
    Hashtbl.find_opt get_scope_buffers uri
    |> Option.map f
    |> Option.value ~default:(IO.return default)


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

      (* one env per document *)
      val get_scope_buffers : (DocumentUri.t, get_scope_info) Hashtbl.t =
        Hashtbl.create 32

      (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
      method private _on_doc : notify_back -> DocumentUri.t -> string -> unit IO.t =
        fun notify_back uri contents ->
          let ((errors, warnings, data) as new_state) = get_scope uri contents in
          Hashtbl.replace get_scope_buffers uri new_state;
          let* () =
            notify_back#send_log_msg
              ~type_:Info
              ("Updating DOC :" ^ DocumentUri.to_string uri)
          in
          let* () =
            match errors with
            | [] -> notify_back#send_log_msg ~type_:Info "No erorrs"
            | _ ->
              let value =
                List.map Main_errors.Formatter.error_json errors |> List.concat
              in
              notify_back#send_log_msg
                ~type_:Info
                ("There are errors:\n *"
                ^ String.concat "\n *"
                @@ List.map (fun (x : Simple_utils.Error.t) -> x.content.message) value)
          in
          (* FIXME and add Warnings*)
          let extract_error_information : Main_errors.all -> (Range.t * string) list =
           fun e ->
            let errs = Main_errors.Formatter.error_json e in
            List.map
              (fun Simple_utils.Error.{ content = { message; location; _ }; _ } ->
                match location with
                | Some location ->
                  ( Option.value
                      ~default:Utils.dummy_range
                      (Utils.location_to_range location)
                  , message )
                | None -> Utils.dummy_range, message)
              errs
          in
          let res =
            errors
            |> List.map extract_error_information
            |> List.concat
            |> List.map (fun (range, message) -> Diagnostic.create ~message ~range ())
          in
          let* () = notify_back#send_diagnostic res in
          let* () =
            match warnings with
            | [] -> notify_back#send_log_msg ~type_:Info "No warnings"
            | _ -> notify_back#send_log_msg ~type_:Info "There are warnings"
          in
          match data with
          | None -> notify_back#send_log_msg ~type_:Info "No result"
          | _ -> notify_back#send_log_msg ~type_:Info "There is result"

      (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
      method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
        let message =
          ShowMessageParams.create ~message:"WELCOME NOT NOTIFICATION!!!" ~type_:Info
        in
        let* () = notify_back#send_notification (ShowMessage message) in
        self#_on_doc notify_back d.uri content

      (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
      method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
        self#_on_doc notify_back d.uri new_content

      (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
      method on_notif_doc_did_close ~notify_back _ : unit Linol_lwt.t =
        let* () = notify_back#send_log_msg ~type_:Info "Closed!!!" in
        Linol_lwt.return ()

      method on_req_hover_
          : notify_back
            -> Position.t
            -> DocumentUri.t
            -> get_scope_info
            -> Hover.t option IO.t =
        fun _notify_back pos uri (_, _, defs) ->
          let open Maybe in
          let< defs, _ = defs in
          let< definition = Requests.get_definition pos uri defs in
          let> str =
            match definition with
            | Variable vdef ->
              return
                (match vdef.t with
                | Core ty -> Format.asprintf "%a" Ast_core.PP.type_content ty.type_content
                | Resolved ty ->
                  Format.asprintf "%a" Ast_typed.PP.type_content ty.type_content
                | Unresolved -> "(* Unresolved Type *)")
            | Type tdef ->
              let< region = Utils.position_of_location tdef.range in
              let< type_definition = Requests.get_definition region uri defs in
              (match type_definition with
              | Type tdef ->
                return @@ Format.asprintf "%a" Ast_core.PP.type_expression tdef.content
              | _ -> failwith "Got a non-type as a type definition.")
            | Module _mdef -> return "Hover for modules is not implemented yet."
          in
          let marked_string : MarkedString.t = { value = str; language = None } in
          let contents = `MarkedString marked_string in
          let hover = Hover.create ~contents () in
          IO.return (Some hover)

      method on_req_formatting_
          : notify_back -> DocumentUri.t -> TextEdit.t list option IO.t =
        fun notify_back uri ->
          let* () =
            notify_back#send_log_msg ~type_:MessageType.Info
            @@ "Formatting request on "
            ^ DocumentUri.to_path uri
          in
          let formatted_text = formatting uri in
          match formatted_text with
          | Ok (result, _) ->
            let formatted_text =
              TextEdit.create ~newText:result ~range:whole_file_range
            in
            IO.return (Some [ formatted_text ])
          | Error _ -> IO.return None

      method on_req_definition_
          : notify_back
            -> Position.t
            -> DocumentUri.t
            -> get_scope_info
            -> Locations.t option IO.t =
        fun _notify_back pos uri (_, _, defs) ->
          let open Maybe in
          let< defs, _ = defs in
          let< definition = Requests.get_definition pos uri defs in
          let region = get_location definition in
          match region with
          | File region -> return (`Location [ region_to_location region ])
          | Virtual _ -> IO.return None

      method on_req_type_definition_
          : notify_back
            -> Position.t
            -> DocumentUri.t
            -> get_scope_info
            -> Locations.t option IO.t =
        fun _notify_back pos uri (_, _, defs) ->
          let open Maybe in
          let< defs, _ = defs in
          let< definition = Requests.get_definition pos uri defs in
          let> location =
            match definition with
            (* It's a term: find its type now. *)
            | Variable vdef ->
              (match vdef.t with
              | Core ty -> return ty.location
              | Resolved ty ->
                IO.return
                @@ Option.map (fun v -> Ligo_prim.Type_var.get_location v) ty.orig_var
              | Unresolved -> IO.return None)
            (* Just replicate the behavior of definition if it's already a type. *)
            | Type tdef -> return tdef.range
            (* Modules have no type definition. *)
            | Module _mdef -> IO.return None
          in
          let< region = Utils.position_of_location location in
          let< type_definition = Requests.get_definition region uri defs in
          let< location =
            match type_definition with
            | Variable _vdef -> None
            | Type tdef -> Some tdef.range
            | Module _mdef -> None
          in
          match location with
          | File region -> return (`Location [ region_to_location region ])
          | Virtual _ -> IO.return None

      method on_req_rename_
          : notify_back
            -> string
            -> Position.t
            -> DocumentUri.t
            -> get_scope_info
            -> WorkspaceEdit.t IO.t =
        fun notify_back new_name pos uri (_, _, defs) ->
          let* value =
            let open Maybe in
            let< defs, _ = defs in
            let< definition = Requests.get_definition pos uri defs in
            let* references =
              Requests.get_all_references
                notify_back
                (get_location definition)
                get_scope_buffers
            in
            let changes =
              List.map
                (fun (file, ranges) ->
                  file, List.map (Requests.rename_reference new_name) ranges)
                references
            in
            return (WorkspaceEdit.create ~changes ())
          in
          IO.return @@ Option.value value ~default:(WorkspaceEdit.create ())

      method on_req_references_
          : notify_back
            -> Position.t
            -> DocumentUri.t
            -> get_scope_info
            -> Location.t list option IO.t =
        fun notify_back pos uri (_, _, defs) ->
          let open Maybe in
          let< defs, _ = defs in
          let< definition = Requests.get_definition pos uri defs in
          let* references =
            Requests.get_all_references
              notify_back
              (get_location definition)
              get_scope_buffers
          in
          let* _ =
            notify_back#send_log_msg ~type_:MessageType.Info
            @@ "On references request on "
            ^ DocumentUri.to_path uri
          in
          let show_reference (uri, ranges) =
            DocumentUri.to_path uri
            ^ "\n"
            ^ String.concat "\n"
            @@ List.map Utils.range_to_string ranges
          in
          let* _ =
            notify_back#send_log_msg ~type_:MessageType.Info
            @@ String.concat "\n"
            @@ List.map show_reference references
          in
          let locations =
            List.flatten
            @@ List.map
                 (fun (file, ranges) ->
                   List.map (fun range -> Location.create ~uri:file ~range) ranges)
                 references
          in
          return locations

      method! config_hover = Some (`Bool true)
      method config_formatting = Some (`Bool true)
      method! config_definition = Some (`Bool true)
      method config_rename = Some (`Bool true)
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
          match r with
          | Client_request.TextDocumentFormatting { textDocument; _ } ->
            let uri = textDocument.uri in
            let notify_back = new notify_back ~uri ~notify_back () in
            self#on_req_formatting_ notify_back uri
          | Client_request.TextDocumentDefinition { textDocument; position; _ } ->
            let uri = textDocument.uri in
            let notify_back = new notify_back ~uri ~notify_back () in
            with_get_scope_info
              get_scope_buffers
              uri
              None
              (self#on_req_definition_ notify_back position uri)
          | Client_request.TextDocumentHover { textDocument; position; _ } ->
            let uri = textDocument.uri in
            let notify_back = new notify_back ~uri ~notify_back () in
            with_get_scope_info
              get_scope_buffers
              uri
              None
              (self#on_req_hover_ notify_back position uri)
          | Client_request.TextDocumentRename { newName; position; textDocument; _ } ->
            let uri = textDocument.uri in
            let notify_back = new notify_back ~uri ~notify_back () in
            with_get_scope_info
              get_scope_buffers
              uri
              (WorkspaceEdit.create ())
              (self#on_req_rename_ notify_back newName position uri)
          | Client_request.TextDocumentReferences { position; textDocument; _ } ->
            let uri = textDocument.uri in
            let notify_back = new notify_back ~uri ~notify_back () in
            with_get_scope_info
              get_scope_buffers
              uri
              None
              (self#on_req_references_ notify_back position uri)
          | Client_request.TextDocumentTypeDefinition { textDocument; position; _ } ->
            let uri = textDocument.uri in
            let notify_back = new notify_back ~uri ~notify_back () in
            with_get_scope_info
              get_scope_buffers
              uri
              None
              (self#on_req_type_definition_ notify_back position uri)
          | _ -> super#on_request ~notify_back ~id r
    end
end
