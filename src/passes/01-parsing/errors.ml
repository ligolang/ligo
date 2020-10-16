(* Vendor dependencies *)

(*open Simple_utils.Display*)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Internal dependencies *)

module CST = Cst.Reasonligo

(* Errors *)

type parse_error = [
  | `Parser_generic of string Region.reg
  | `Parser_wrong_function_arguments of CST.expr
  | `Parser_invalid_wild of CST.expr
  ]

let stage = "parser"

let generic reg = `Parser_generic reg
let wrong_function_arguments expr = `Parser_wrong_function_arguments expr
let invalid_wild expr = `Parser_invalid_wild expr

let wrong_function_msg =
  "It looks like you are defining a function, \
   however we do not\n\
   understand the parameters declaration.\n\
   Examples of valid functions:\n\
   let x = (a: string, b: int) : int => 3;\n\
   let tuple = ((a, b): (int, int)) => a + b; \n\
   let x = (a: string) : string => \"Hello, \" ++ a;\n"

let wild_pattern_msg =
  "It looks like you are using a catch-all pattern where it cannot be used"

let error_ppformat :
      display_format:(string Display.display_format) ->
      Format.formatter ->
      parse_error ->
      unit =
  fun ~display_format f a ->
  match display_format with
    Human_readable | Dev -> (
      match a with
        `Parser_generic reg ->
           Snippet.pp_lift f reg.Region.region;
           Format.pp_print_string f reg.Region.value
      | `Parser_wrong_function_arguments expr ->
           let loc = Format.asprintf "%a"
             Snippet.pp_lift @@ CST.expr_to_region expr in
           let s = Format.asprintf "%s\n%s" loc wrong_function_msg
           in Format.pp_print_string f s
      | `Parser_invalid_wild expr ->
           let loc = Format.asprintf "%a"
             Snippet.pp_lift @@ CST.expr_to_region expr in
           let s = Format.asprintf "%s\n%s" loc wild_pattern_msg
           in Format.pp_print_string f s)

let error_jsonformat : parse_error -> Yojson.Safe.t =
  fun error ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error");
      ("stage",  `String stage);
      ("content", content )] in
  match error with
    `Parser_generic reg ->
       let loc =
         Format.asprintf "%a" Location.pp_lift @@ reg.Region.region in
       let content = `Assoc [
         ("message",  `String reg.Region.value);
         ("location", `String loc)]
    in json_error ~stage ~content
  | `Parser_wrong_function_arguments expr ->
       let loc = Format.asprintf "%a" Location.pp_lift @@
                   CST.expr_to_region expr in
       let content = `Assoc [
         ("message", `String wrong_function_msg);
         ("location", `String loc)]
       in json_error ~stage ~content
  | `Parser_invalid_wild expr ->
       let loc =
         Format.asprintf "%a" Location.pp_lift @@ CST.expr_to_region expr in
       let content = `Assoc [
         ("message", `String wild_pattern_msg);
         ("location", `String loc); ]
       in json_error ~stage ~content
