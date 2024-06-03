module Location = Simple_utils.Location
module Snippet = Simple_utils.Snippet
open Simple_utils.Display

let stage = "expansion"

type expansion_error =
  [ `Expansion_corner_case of string * string
  | `Expansion_cannot_compile_texists of Ast_typed.type_expression * Location.t
  ]
[@@deriving poly_constructor { prefix = "expansion_" }]

let corner_case ~loc desc = corner_case loc desc

let error_ppformat
    :  display_format:string display_format -> no_colour:bool -> Format.formatter
    -> expansion_error -> unit
  =
 fun ~display_format ~no_colour f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Expansion_corner_case (loc, desc) ->
      Format.fprintf f "@[<hv>An expansion corner case occurred %s:@.%s@]" loc desc
    | `Expansion_cannot_compile_texists (type_, loc) ->
      Format.fprintf
        f
        "@[<hv>Underspecified type \"%a\".@.Please add additional annotations.%a@]"
        Ast_typed.PP.type_expression
        type_
        (Snippet.pp ~no_colour)
        loc)


let error_json : expansion_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  let message =
    Format.asprintf "%a" (error_ppformat ~display_format:Human_readable ~no_colour:true) e
  in
  let content = make_content ~message () in
  make ~stage ~content
