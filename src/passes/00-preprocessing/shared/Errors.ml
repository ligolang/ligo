(* Preprocessing errors for the compiler *)

let stage = "preprocessing"

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Errors *)

type t = [
  `Preprocessing_generic of string Region.reg
]

type error = t

let generic reg = `Preprocessing_generic reg

(* Colour snippet *)

let error_ppformat :
      display_format:(string Display.display_format) ->
      no_colour:bool ->
      Format.formatter ->
      error ->
      unit =
  fun ~display_format ~no_colour format error ->
  match display_format with
    Human_readable | Dev ->
      match error with
        `Preprocessing_generic Region.{region; value} ->
           Snippet.pp_lift ~no_colour format region;
           Format.pp_print_string format value

(* JSON *)

let error_json : error -> Simple_utils.Error.t =
  fun error ->
    let open Simple_utils.Error  in
    match error with
      `Preprocessing_generic Region.{region; value} ->
        let location = Location.lift region in
        let message = value in
        let content = make_content ~message ~location () in
        make ~stage ~content
