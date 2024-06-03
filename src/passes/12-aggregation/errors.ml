module Display = Simple_utils.Display
module Location = Simple_utils.Location
module Snippet = Simple_utils.Snippet
module Ligo_Error = Simple_utils.Error

let stage = "aggregation"

type aggregation_error =
  [ `Aggregation_corner_case of string
  | `Aggregation_redundant_pattern of Location.t
  | `Aggregation_cannot_compile_erroneous_expression of Ast_typed.expression * Location.t
  ]
[@@deriving poly_constructor { prefix = "aggregation_" }]

let error_ppformat
    :  display_format:string Display.display_format -> no_colour:bool -> Format.formatter
    -> aggregation_error -> unit
  =
 fun ~display_format ~no_colour f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Aggregation_corner_case desc ->
      Format.fprintf f "@[<hv>An aggregation corner case occurred:@.%s@]" desc
    | `Aggregation_cannot_compile_erroneous_expression (_expr, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Cannot compile erroneous expression.@]"
        (Snippet.pp ~no_colour)
        loc
    | `Aggregation_redundant_pattern loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Redundant pattern matching@]"
        (Snippet.pp ~no_colour)
        loc)


let error_json : aggregation_error -> Ligo_Error.t =
 fun e ->
  let open Ligo_Error in
  match e with
  | `Aggregation_corner_case desc ->
    let message =
      Format.sprintf "@[<hv>An aggregation corner case occurred:@.%s@]" desc
    in
    let content = make_content ~message () in
    make ~stage ~content
  | `Aggregation_cannot_compile_erroneous_expression (_expr, loc) ->
    let message = "Cannot compile erroneous expression." in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Aggregation_redundant_pattern location ->
    let message = "Redundant pattern matching" in
    let content = make_content ~message ~location () in
    make ~stage ~content
