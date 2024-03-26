open Handler
open Lsp_helpers

(** Convenient wrapper for declaring commands handled by server. *)
module Command = struct
  module Lsp_command = Lsp_helpers.Command

  type 'a t =
    { id : string
    ; args_encoder : 'a -> Yojson.Safe.t list
    ; args_decoder : Yojson.Safe.t list -> ('a, string) result
    }

  let compile ~(command : 'a t) ~(title : string) ~(arguments : 'a) : Lsp_command.t =
    Command.create
      ~title
      ~command:command.id
      ~arguments:(command.args_encoder arguments)
      ()


  module Match = struct
    type 'r command_handler = LspHandler : 'a t * ('a -> 'r) -> 'r command_handler

    let arm ~(command : 'a t) ~(handler : 'a -> 'r) : 'r command_handler =
      LspHandler (command, handler)


    type 'r or_error =
      | Unknown_command
      | Decode_error of string
      | Ok of 'r

    let create_matcher (handlers : 'r command_handler list)
        : command:string -> arguments:Yojson.Safe.t list -> 'r or_error
      =
      let module HandlersMap = Map.Make (String) in
      let handler_id = function
        | LspHandler (c, _) -> c.id
      in
      let handlers_map =
        match HandlersMap.of_alist (List.map handlers ~f:(fun h -> handler_id h, h)) with
        | `Ok hs -> hs
        | `Duplicate_key key ->
          raise @@ Invalid_argument (sprintf "Duplicate handler id `%s`" key)
      in
      fun ~command ~arguments ->
        match HandlersMap.find handlers_map command with
        | None -> Unknown_command
        | Some (LspHandler (cmd, handler)) ->
          (match cmd.args_decoder arguments with
          | Error err -> Decode_error err
          | Ok args -> Ok (handler args))
  end
end

type storage_var_position =
  { file : Path.t
  ; line : int
  ; (* Version of the document at which `line` is actual *)
    document_version : Ligo_interface.document_version
  }

let create_attr_text (name : string) (syntax : Syntax_types.t) : string =
  match syntax with
  | CameLIGO -> Format.sprintf "[%@%s]" name
  | JsLIGO -> Format.sprintf "%@%s" name


let execute_add_tzip16_attr (storage_var_position : storage_var_position)
    : Yojson.Safe.t handler
  =
  let@ () =
    send_debug_msg
    @@ Format.asprintf
         "Adding tzip16 attribute in %a:%d"
         Path.pp
         storage_var_position.file
         storage_var_position.line
  in
  let@ syntax = get_syntax_exn storage_var_position.file in
  let inserted_text =
    Format.sprintf "%s@." @@ create_attr_text "tzip16_compatible" syntax
  in
  let edit =
    ApplyWorkspaceEditParams.create
      ~label:"Add tzip16 attribute"
      ()
      ~edit:
        (WorkspaceEdit.create
           ~documentChanges:
             [ `TextDocumentEdit
                 (TextDocumentEdit.create
                    ~edits:
                      [ `TextEdit
                          (TextEdit.create
                             ~newText:inserted_text
                               (* TODO: actually, we want not 0 here, see #2151 *)
                             ~range:(Range.Construct.point storage_var_position.line 0))
                      ]
                    ~textDocument:
                      (OptionalVersionedTextDocumentIdentifier.create
                         ~uri:(DocumentUri.of_path storage_var_position.file)
                         ~version:storage_var_position.document_version
                         ()))
             ]
           ())
  in
  let@ () =
    send_request (WorkspaceApplyEdit edit) (function
        | Error error ->
          let@ () = send_log_msg ~type_:Error error.message in
          send_message ~type_:Error "Failed to apply code change. Check logs."
        | Ok { applied; failureReason; failedChange } ->
          when_ (not applied)
          @@ let@ () =
               Option.value_map ~default:pass ~f:(send_log_msg ~type_:Error) failureReason
             in
             let@ () =
               Option.value_map
                 ~default:pass
                 ~f:(send_log_msg ~type_:Error <@ Format.sprintf "Failed change: %d")
                 failedChange
             in
             send_message ~type_:Error "Failed to apply code change. Check logs.")
  in
  return `Null


module Ligo_lsp_commands = struct
  (* On encoding/decoding command arguments, there is nothing wrong in passing
     one large JSON argument all the time. But since LSP assumes passing a list
     of arguments, and there might be minor reasons to make these commands
     public, let's pass each command argument as a separate simple list element.
     *)

  let add_tzip16_attr ~(normalize : string -> Path.t) : storage_var_position Command.t =
    { id = "add-tzip16-addr"
    ; args_encoder =
        (fun args ->
          [ `String (Path.to_string args.file)
          ; `Int args.line
          ; `Int args.document_version
          ])
    ; args_decoder =
        (function
        | [ `String p; `Int l; `Int v ] ->
          Ok { file = normalize p; line = l; document_version = v }
        | _ -> Error "Invalid argument types")
    }


  let match_command ~(normalize : string -> Path.t)
      :  command:string -> arguments:Yojson.Safe.t list
      -> Yojson.Safe.t handler Command.Match.or_error
    =
    let open Command.Match in
    create_matcher
      [ arm ~command:(add_tzip16_attr ~normalize) ~handler:execute_add_tzip16_attr ]
end

let on_execute_command ~(command : string) ?(arguments : Yojson.Safe.t list = []) ()
    : Yojson.Safe.t handler
  =
  let@ normalize = ask_normalize in
  match Ligo_lsp_commands.match_command ~normalize ~command ~arguments with
  | Ok action -> action
  | Unknown_command ->
    let@ () =
      send_log_msg ~type_:Error @@ sprintf "Got an unknown command `%s`" command
    in
    return `Null
  | Decode_error msg ->
    let@ () =
      send_log_msg ~type_:Error
      @@ sprintf "Failed to decode arguments for command `%s`: %s" command msg
    in
    return `Null
