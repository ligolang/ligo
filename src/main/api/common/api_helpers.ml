open Simple_utils.Display
open Simple_utils
module Trace = Simple_utils.Trace

let toplevel
    :  ?warning_as_error:bool -> minify_json:bool -> display_format:ex_display_format
    -> no_colour:bool -> displayable -> ('value * 'w, _) result -> _
  =
 fun ?(warning_as_error = false) ~minify_json ~display_format ~no_colour disp value ->
  let (Ex_display_format t) = display_format in
  let format_yojson (yojson : json) : string =
    if minify_json
    then Yojson.Safe.to_string yojson
    else Yojson.Safe.pretty_to_string yojson
  in
  let as_str : string =
    match t with
    | Human_readable -> convert ~display_format:t ~no_colour disp
    | Dev -> convert ~display_format:t ~no_colour disp
    | Json -> format_yojson @@ convert ~display_format:t ~no_colour disp
  in
  let warns =
    match value with
    | Ok (_, w) -> w
    | Error (_, w) -> w
  in
  let warns =
    List.map warns ~f:(fun value ->
        match t with
        | (Human_readable | Dev) as s ->
          convert
            ~display_format:s
            ~no_colour
            (Displayable { value; format = Main_warnings.format })
        | Json ->
          format_yojson
          @@ convert
               ~display_format:t
               ~no_colour
               (Displayable { value; format = Main_warnings.format }))
  in
  let warns_str = String.concat ~sep:"\n" warns in
  if (not (List.is_empty warns)) && warning_as_error
  then Error (warns_str ^ as_str, warns_str)
  else (
    match value with
    | Ok _ -> Ok (as_str, warns_str)
    | Error _ -> Error (as_str, warns_str))


(** This heuristic checks whether the given file refers to a file defined within
    a LIGO registry package. *)
let is_packaged (file : string) : bool =
  (* Here we use a heuristic: if the file is defined within any directory called
      ".ligo", we suppose that it was imported. *)
  List.mem (String.split_on_chars ~on:[ '\\'; '/' ] file) ".ligo" ~equal:String.( = )


let list_directory ?(include_library = false) ?syntax (dir : string) : string list =
  let rec aux ?(include_library = include_library) (res : string list)
      : string list -> string list
    = function
    | f :: fs when Caml.Sys.is_directory f ->
      let is_packaged = is_packaged @@ FilePath.make_relative dir f in
      if (not include_library) && is_packaged
      then aux res fs
      else
        Caml.Sys.readdir f
        |> Array.to_list
        |> List.map ~f:(Filename.concat f)
        |> List.append fs
        (* We don't want to display the completions for nested dependencies *)
        |> aux ~include_library:(include_library && not is_packaged) res
    | f :: fs ->
      let _, ext = Filename.split_extension f in
      (match Syntax.of_ext_opt ext with
      | Some syn
        when Option.is_none syntax || Option.mem syntax syn ~equal:Syntax_types.equal ->
        aux (f :: res) fs
      | _ -> aux res fs)
    | [] -> res
  in
  aux [] [ dir ]
