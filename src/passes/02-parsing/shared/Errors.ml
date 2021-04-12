(* Parsing errors for the compiler *)

let stage = "parsing"

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Internal dependencies *)

module CST = Cst_reasonligo.CST

(* Errors *)

type t = [
  `Parsing_generic      of string Region.reg
| `Parsing_invalid_wild of CST.expr
]

type error = t

let generic reg       = `Parsing_generic reg
let invalid_wild expr = `Parsing_invalid_wild expr

let wrong_function_msg =
  "It looks like you are defining a function, \
   however we do not\n\
   understand the parameters declaration.\n\
   Examples of valid functions:\n\
   let x = (a: string, b: int) : int => 3;\n\
   let tuple = ((a, b): (int, int)) => a + b; \n\
   let x = (a: string) : string => \"Hello, \" ++ a;\n"

let wild_pattern_msg =
  "It looks like you are using a catch-all pattern \
   where it cannot be used"

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
        `Parsing_generic Region.{value; region} ->
           Snippet.pp_lift format region;
           Format.pp_print_string format value
      | `Parsing_invalid_wild expr ->
         let loc = Format.asprintf "%a" Snippet.pp_lift
                   @@ CST.expr_to_region expr in
         Format.(pp_print_string format @@
                   asprintf "%s\n%s" loc wild_pattern_msg)

(* JSON *)

let error_jsonformat : error -> Yojson.Safe.t =
  fun error ->
  let json_error ~stage ~content =
    `Assoc [
      ("status",  `String "error");
      ("stage",   `String stage);
      ("content",  content)] in
  match error with
    `Parsing_generic Region.{value; region} ->
       let loc = Location.lift @@ region in
       let content = `Assoc [
         ("message",  `String value);
         ("location", Location.to_yojson loc)]
       in json_error ~stage ~content
  | `Parsing_invalid_wild expr ->
       let loc = Location.lift @@ CST.expr_to_region expr in
       let content =
         `Assoc [
            ("message", `String wild_pattern_msg);
            ("location", Location.to_yojson loc)]
       in json_error ~stage ~content
