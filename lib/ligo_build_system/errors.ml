module Display = Simple_utils.Display
module Ligo_Error = Simple_utils.Error

type t =
  [ `Build_dependency_cycle of string
  | `Build_corner_case of string * string (* TO REMOVE *)
  ]

let build_dependency_cycle (s : string) = `Build_dependency_cycle s
let build_corner_case (loc : string) (msg : string) = `Build_corner_case (loc, msg)

let error_ppformat
    :  display_format:string Display.display_format -> no_colour:bool -> Format.formatter
    -> t -> unit
  =
 fun ~display_format ~no_colour:_ f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Build_dependency_cycle trace ->
      Format.fprintf f "@[<hv>Dependency cycle detected :@, %s@]" trace
    | `Build_corner_case (loc, msg) ->
      Format.fprintf f "@[<hv>Building corner case at %s : %s@]" loc msg)

let error_json : t -> Ligo_Error.t =
 fun e ->
  let stage = "build system" in
  match e with
  | `Build_dependency_cycle trace ->
    let message = Format.asprintf "@[<hv>Dependency cycle detected :@, %s@]" trace in
    let content = Ligo_Error.make_content ~message () in
    Ligo_Error.make ~stage ~content
  | `Build_corner_case (loc, msg) ->
    let message = Format.asprintf "@[<hv>Building corner case at %s : %s@]" loc msg in
    let content = Ligo_Error.make_content ~message () in
    Ligo_Error.make ~stage ~content

let error_jsonformat : t -> Yojson.Safe.t =
 fun e -> Simple_utils.Error.to_yojson (error_json e)

let error_format : _ Display.format = { pp = error_ppformat; to_json = error_jsonformat }
