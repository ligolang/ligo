(* Vendor dependencies *)

(*open Simple_utils.Display*)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Errors *)

type build_error = [
  | `Dependency_cycle of string
  | `Building_corner_case of string * string
  | `Compiler_error of Main_errors.all
  ]

let stage = "build_system"
let corner_case_msg () =
  "Sorry, we don't have a proper error message for this error. Please report \
   this use case so we can improve on this."

let dependency_cycle trace = `Dependency_cycle trace
let corner_case ~loc  message = `Building_corner_case (loc,message)
let compiler_error e = `Compiler_error e

let error_ppformat :
      display_format:(string Display.display_format) ->
      Format.formatter ->
      build_error ->
      unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Dependency_cycle trace ->
          let s = Format.asprintf "Dependency cycle detected : \n %s" trace
          in Format.pp_print_string f s
    | `Building_corner_case (loc,msg) ->
      let s = Format.asprintf "Stacking corner case at %s : %s.\n%s"
        loc msg (corner_case_msg ()) in
      Format.pp_print_string f s ;
    | `Compiler_error e ->
      Main_errors.Formatter.error_ppformat ~display_format f e
    )

let error_jsonformat : build_error -> Yojson.Safe.t =
  fun error ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error");
      ("stage",  `String stage);
      ("content", content )] in
  match error with
    `Dependency_cycle trace ->
       let content = `Assoc [
         ("message",  `String "Dependency cycle detected");
         ("cycle"  , `String trace);
       ]
    in json_error ~stage ~content
  | `Building_corner_case (loc,msg) ->
    let content = `Assoc [
      ("location", `String loc);
      ("message", `String msg); ] in
    json_error ~stage ~content
  | `Compiler_error e -> Main_errors.Formatter.error_jsonformat e

let error_format : _ Display.format = {
  pp = error_ppformat;
  to_json = error_jsonformat;
}
