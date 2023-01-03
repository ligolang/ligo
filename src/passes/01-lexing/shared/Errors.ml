(* Lexing errors for the compiler *)

let stage = "lexing"

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Errors *)

type t = [
  `Lexing_generic of string Region.reg
| `Unbalanced_token of Region.t
]

type error = t

let generic reg = `Lexing_generic reg

let unbalanced t = `Unbalanced_token t

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
        `Lexing_generic Region.{region; value} ->
           Snippet.pp_lift ~no_colour format region;
           Format.pp_print_string format value
      | `Unbalanced_token region ->
          Snippet.pp_lift ~no_colour format region;
          Format.pp_print_string format "Unbalanced token. Please ensure that left braces/parentheses are properly balanced with right braces/parentheses."

(* JSON *)

let error_json : error -> Simple_utils.Error.t =
  fun error ->
    let open Simple_utils.Error in
    match error with
      `Lexing_generic Region.{region; value} ->
         let location = Location.lift region in
         let message = value in
         let content = make_content ~message ~location () in
         make ~stage ~content
    | `Unbalanced_token region ->
        let location = Location.lift region in
        let message = "Unbalanced token" in
        let content = make_content ~message ~location () in
        make ~stage ~content
