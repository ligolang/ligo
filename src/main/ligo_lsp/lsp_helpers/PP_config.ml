(* TODO move this file outside LSP, support this config in [ligo print pretty] *)

(** Config that we'll parse from formatter configuration file. 
    The scheme is described in docs for editor extensions like tools/vscode/README.md,
    those docs should be updated if new options are added *)
type t =
  { tab_width : int option [@key "tabWidth"] [@default None]
  ; print_width : int option [@key "printWidth"] [@default None]
  }
[@@deriving of_yojson, show]

(** name for formatter configuration file, we'll look for it in current dir and 
    its parents when user wants to format some file. 
    We search for this file and parse it while handling each formatting / range formatting request.
    If changing this, please update the file name in editor extension docs,
    currently it means in tools/vscode/README.md *)
let config_file_name = ".ligopretty"

(** Expects absolute path to LIGO file, searches for pretty printer configuration file
      in its directory and parent directories. Tries to parse a [Yojson.Safe.t] from this file.*)
let get_config_json
    : Path.t -> [ `Json of Yojson.Safe.t | `File_not_found | `Decode_error of string ]
  =
 fun path ->
  let yojson_from_file_safe file =
    try `Json (Yojson.Safe.from_file (Path.to_string file)) with
    | Yojson.Json_error exn -> `Decode_error exn
  in
  match Path.find_file_in_dir_and_parents (Path.dirname path) config_file_name with
  | Some file -> yojson_from_file_safe file
  | None -> `File_not_found


(** Expects absolute path to LIGO file, searches for pretty printer configuration file
      in its directory and parent directories. Tries to parse a [PP_config.t] from this file.*)
let get_config
    :  Path.t
    -> [ `PP_config of t
       | `File_not_found
       | `Decode_error of string
       | `Wrong_json of string
       ]
  =
 fun path ->
  match get_config_json path with
  | (`File_not_found | `Decode_error _) as x -> x
  | `Json json ->
    (match of_yojson json with
    | Ok x -> `PP_config x
    | Error err -> `Wrong_json err)
