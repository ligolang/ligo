open Simple_utils.Display
open Simple_utils
module Trace = Simple_utils.Trace

let toplevel
    :  ?warning_as_error:bool -> minify_json:bool -> display_format:ex_display_format
    -> no_colour:bool -> displayable -> ('value * _ * 'w, _) result -> _
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
    | Ok (_, _, w) -> w
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


let ligo_package_dir : string = ".ligo"

(** This heuristic checks whether the given file refers to a file defined within
    a LIGO registry package. *)
let is_packaged (file : string) : bool =
  (* Here we use a heuristic: if the file is defined within any directory called
      ".ligo", we suppose that it was imported. *)
  List.mem (Filename.parts file) ligo_package_dir ~equal:Filename.equal


let list_directory ?(include_library = false) ?syntax (dir : string) : string list =
  let rec aux (res : string list) : string list -> string list = function
    | f :: fs when Caml.Sys.is_directory f ->
      let parts = Filename.parts f in
      let number_of_deps = List.count parts ~f:(Filename.equal ligo_package_dir) in
      let is_hidden = List.exists parts ~f:(String.is_prefix ~prefix:".") in
      (* We don't want to display completions and references for nested dependencies. *)
      let is_packaged = number_of_deps = 1 in
      let is_nested_package = number_of_deps > 1 in
      if is_nested_package
         || ((not include_library) && is_packaged)
         || ((not is_packaged) && is_hidden)
      then aux res fs
      else
        Caml.Sys.readdir f
        |> Array.to_list
        |> List.map ~f:(Filename.concat f)
        |> List.append fs
        |> aux res
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
