(* Parsing errors for the compiler *)

let stage = "parsing"

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Errors *)

type t = [`Parsing of string Region.reg]

type error = t

(* Colour snippet *)

type pp_formater =
  display_format:(string Display.display_format) ->
  no_colour:bool ->
  Format.formatter ->
  t ->
  unit

let error_ppformat : pp_formater =
  fun ~display_format ~no_colour format error ->
  match display_format with
    Human_readable | Dev ->
      match error with
        `Parsing Region.{value; region} ->
           Snippet.pp_lift ~no_colour format region;
           Format.pp_print_string format value

let to_ppformat = error_ppformat

(* JSON *)

let error_json : t -> Simple_utils.Error.t =
  fun error ->
  let open Simple_utils.Error in
  match error with
    `Parsing Region.{value; region} ->
      let location = Location.lift region in
      let message = value in
      let content = make_content ~message ~location () in
      make ~stage ~content

module ErrorPrefix = struct
  let prefix = "⚠You are not supposed to see this prefix⚠"
  let add s = prefix ^ s
  let remove s =
    match String.chop_prefix ~prefix s with
    | Some s -> s
    | None -> s
  let is_contained = String.is_substring ~substring:prefix
end
