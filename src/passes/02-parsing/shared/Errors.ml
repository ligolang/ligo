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
  Format.formatter ->
  t ->
  unit

let error_ppformat : pp_formater =
  fun ~display_format format error ->
  match display_format with
    Human_readable | Dev ->
      match error with
        `Parsing Region.{value; region} ->
           Snippet.pp_lift format region;
           Format.pp_print_string format value

let to_ppformat = error_ppformat

(* JSON *)

let error_jsonformat : t -> Simple_utils.Error.t =
  fun error ->
  let open Simple_utils.Error in
  match error with
    `Parsing Region.{value; region} ->
      let location = Location.lift region in
      let message = value in
      let content = make_content ~message ~location () in
      make ~stage ~content
