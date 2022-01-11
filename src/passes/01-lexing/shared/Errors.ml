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
      Format.formatter ->
      error ->
      unit =
  fun ~display_format format error ->
  match display_format with
    Human_readable | Dev ->
      match error with
        `Lexing_generic Region.{region; value} ->
           Snippet.pp_lift format region;
           Format.pp_print_string format value
      | `Unbalanced_token region ->
          Snippet.pp_lift format region;
          Format.pp_print_string format "Unbalanced token. Please ensure that left braces/parenthesis are properly balanced with right braces/parenthesis."

(* JSON *)

let error_jsonformat : error -> Yojson.Safe.t =
  fun error ->
    let json_error ~stage ~content =
      `Assoc [
         ("status", `String "error");
         ("stage",  `String stage);
         ("content", content)] in
    match error with
      `Lexing_generic Region.{region; value} ->
         let loc =
           Format.asprintf "%a" Location.pp_lift @@ region in
         let content =
           `Assoc [
              ("message",  `String value);
              ("location", `String loc)]
         in json_error ~stage ~content
    | `Unbalanced_token region ->
        let loc =
          Format.asprintf "%a" Location.pp_lift @@ region in
        let value = "Unbalanced token" in
        let content =
          `Assoc [
            ("message",  `String value);
            ("location", `String loc)]
        in json_error ~stage ~content
