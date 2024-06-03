open Core
open Handler
open Lsp_helpers

(** Gets a [pp_config], notifying the user if the syntax used in this file is incorrect. *)
let try_get_pp_config : Path.t -> PP_config.t option Handler.t =
 fun path ->
  let open Handler.Let_syntax in
  match PP_config.get_config path with
  | `PP_config t ->
    let%bind () = send_debug_msg @@ "Using config from " ^ Path.to_string path in
    return @@ Some t
  | `File_not_found -> return None
  | (`Decode_error _ | `Wrong_json _) as err ->
    let msg =
      match err with
      | `Decode_error err ->
        "Can't parse JSON from  " ^ PP_config.config_file_name ^ " file: " ^ err
      | `Wrong_json err ->
        "The " ^ PP_config.config_file_name ^ " file contains malformed JSON data: " ^ err
    in
    let%bind () = send_message ?type_:(Some Error) msg in
    return None

(** Gets the [pp_mode] from the provided [pp_config], notifying the user if the syntax
    used in this file is incorrect. If there is no config, then uses default values. *)
let get_pp_mode : Path.t -> FormattingOptions.t -> Pretty.pp_mode Handler.t =
 fun file { tabSize = optsTabSize; _ } ->
  let open Handler.Let_syntax in
  let%bind pp_config_opt = try_get_pp_config file in
  let indent =
    (* If pp_config exists and corresponding field is nonempty we always use it *)
    match Option.bind ~f:(fun pp_config -> pp_config.tab_width) pp_config_opt with
    | Some w -> w
    | None -> optsTabSize
  in
  let%bind width =
    match Option.bind ~f:(fun pp_config -> pp_config.print_width) pp_config_opt with
    | Some w -> return w
    | None ->
      let%bind lsp_config_width = ask_config >>| fun x -> x.max_line_width in
      return
      @@ Option.value
           ~default:Helpers_pretty.default_line_width_for_formatted_file
           lsp_config_width
  in
  return Pretty.{ indent; width }

(* FIXME #1765: add support for configuration file, remove code duplication with range formatting *)

(** Runs the handler for document formatting. This request is normally triggered manually
    by the user, but it may also be triggered from some configuration (e.g., format on
    save). *)
let on_req_formatting : Path.t -> FormattingOptions.t -> TextEdit.t list option Handler.t =
 fun file opts ->
  let open Let_syntax in
  let%bind pp_mode = get_pp_mode file opts in
  let%bind () =
    send_debug_msg
    @@ Format.asprintf
         "Formatting request on %s, mode: %a"
         (Path.to_string file)
         Pretty.pp_pp_mode
         pp_mode
  in
  if Helpers_file.is_packaged @@ Path.to_string file
  then (
    let%bind () =
      send_message ~type_:Error @@ "Can not format a file from an imported package."
    in
    return None)
  else (
    let on_error _err =
      send_message ~type_:Error @@ "Can not format a file with syntax errors"
    in
    with_cst ~strict:true ~on_error file ~default:None
    @@ fun cst ->
    let result = Pretty.pretty_print_cst pp_mode ~dialect_cst:cst in
    return @@ Some [ TextEdit.create ~newText:result ~range:Range.whole_file ])
