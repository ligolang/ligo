module Var = Simple_utils.Var
module Tree = Simple_utils.Tree
module Snippet = Simple_utils.Snippet
module Location = Simple_utils.Location
open Simple_utils.Display
open Ligo_prim

type spilling_error =
  [ `Spilling_corner_case of string * string
  | `Spilling_no_type_variable of Type_var.t
  | `Spilling_unsupported_pattern_matching of Location.t
  | `Spilling_unsupported_recursive_function of Location.t * Value_var.t
  | `Spilling_wrong_mini_c_value of Ast_aggregated.type_expression * Mini_c.value
  | `Spilling_bad_decompile of Mini_c.value
  | `Spilling_could_not_parse_raw_michelson of Location.t * string
  | `Spilling_raw_michelson_must_be_seq of
    Location.t * (Location.t, string) Tezos_micheline.Micheline.node
  ]
[@@deriving poly_constructor { prefix = "spilling_" }]

let stage = "spilling"
let corner_case ~loc desc = corner_case loc desc

let corner_case_message () =
  "Sorry, we don't have a proper error message for this error. Please report this use \
   case so we can improve on this."


let error_ppformat
    :  display_format:string display_format -> no_colour:bool -> Format.formatter
    -> spilling_error -> unit
  =
 fun ~display_format ~no_colour f a ->
  let snippet_pp = Snippet.pp ~no_colour in
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Spilling_corner_case (loc, desc) ->
      let s =
        Format.asprintf "%s\n corner case: %s\n%s" loc desc (corner_case_message ())
      in
      Format.pp_print_string f s
    | `Spilling_no_type_variable tv ->
      let s =
        Format.asprintf
          "%a@.Type \"%a\" not found (should not happen and be caught earlier)."
          snippet_pp
          (Type_var.get_location tv)
          Type_var.pp
          tv
      in
      Format.pp_print_string f s
    | `Spilling_unsupported_pattern_matching loc ->
      let s =
        Format.asprintf
          "%a@.Invalid pattern matching.@Tuple patterns are not (yet) supported."
          snippet_pp
          loc
      in
      Format.pp_print_string f s
    | `Spilling_unsupported_recursive_function (loc, var) ->
      let s =
        Format.asprintf
          "%a@.Invalid recursive function \"%a\".@.A recursive function can only have \
           one argument."
          snippet_pp
          loc
          Value_var.pp
          var
      in
      Format.pp_print_string f s
    | `Spilling_wrong_mini_c_value (expected, actual) ->
      let s =
        Format.asprintf
          "Invalid type.@.Expected \"%a\",@.but got \"%a\"."
          Ast_aggregated.PP.type_expression
          expected
          Mini_c.PP.value
          actual
      in
      Format.pp_print_string f s
    | `Spilling_bad_decompile bad ->
      let s = Format.asprintf "Cannot untranspile: %a" Mini_c.PP.value bad in
      Format.pp_print_string f s
    | `Spilling_could_not_parse_raw_michelson (loc, code) ->
      Format.fprintf
        f
        "@[<hv>%a@.Could not parse raw Michelson:@.\"%s\".@]"
        snippet_pp
        loc
        code
    | `Spilling_raw_michelson_must_be_seq (loc, code) ->
      let open Tezos_micheline.Micheline in
      let open Tezos_micheline.Micheline_printer in
      Format.fprintf
        f
        "@[<hv>%a@.Raw Michelson must be seq (with curly braces {}), got: %a.@]"
        snippet_pp
        loc
        print_expr
        (printable (fun s -> s) (strip_locations code)))


let error_json : spilling_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Spilling_corner_case (loc, desc) ->
    let message =
      Format.asprintf "%s\n corner case: %s\n%s" loc desc (corner_case_message ())
    in
    let content = make_content ~message () in
    make ~stage ~content
  | `Spilling_no_type_variable tv ->
    let message =
      Format.asprintf
        "Type \"%a\" not found (should not happen and be caught earlier)."
        Type_var.pp
        tv
    in
    let location = Type_var.get_location tv in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Spilling_unsupported_pattern_matching location ->
    let message =
      Format.sprintf "Invalid pattern matching.@Tuple patterns are not (yet) supported."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Spilling_unsupported_recursive_function (location, var) ->
    let message =
      Format.asprintf
        "Invalid recursive function \"%a\".@.A recursive function can only have one \
         argument."
        Value_var.pp
        var
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Spilling_wrong_mini_c_value (expected, actual) ->
    let message =
      Format.asprintf
        "Invalid type.@.Expected \"%a\",@.but got \"%a\"."
        Ast_aggregated.PP.type_expression
        expected
        Mini_c.PP.value
        actual
    in
    let content = make_content ~message () in
    make ~stage ~content
  | `Spilling_bad_decompile bad ->
    let message = Format.asprintf "Cannot untranspile: %a" Mini_c.PP.value bad in
    let content = make_content ~message () in
    make ~stage ~content
  | `Spilling_could_not_parse_raw_michelson (location, code) ->
    let message = Format.asprintf "Could not parse raw Michelson:@.\"%s\"." code in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Spilling_raw_michelson_must_be_seq (location, code) ->
    let open Tezos_micheline in
    let message =
      Format.asprintf
        "Raw Michelson must be seq (with curly braces {}), got: %a."
        Micheline_printer.print_expr
        (Micheline_printer.printable Fn.id (Micheline.strip_locations code))
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
