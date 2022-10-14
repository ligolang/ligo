module Var      = Simple_utils.Var
module Tree     = Simple_utils.Tree
module Snippet  = Simple_utils.Snippet
module Location = Simple_utils.Location
open Simple_utils.Display
open Ligo_prim

type spilling_error = [
  | `Spilling_corner_case of string * string
  | `Spilling_no_type_variable of Type_var.t
  | `Spilling_unsupported_pattern_matching of Location.t
  | `Spilling_unsupported_recursive_function of Location.t * Value_var.t
  | `Spilling_wrong_mini_c_value of Ast_aggregated.type_expression * Mini_c.value
  | `Spilling_bad_decompile of Mini_c.value
  | `Spilling_could_not_parse_raw_michelson of Location.t * string
  | `Spilling_raw_michelson_must_be_seq of Location.t * (Location.t, string) Tezos_micheline.Micheline.node
  ] [@@deriving poly_constructor { prefix = "spilling_" }]

let stage = "spilling"

let corner_case ~loc desc = corner_case loc desc
let corner_case_message () =
  "Sorry, we don't have a proper error message for this error. Please report \
  this use case so we can improve on this."

let error_ppformat : display_format:string display_format ->
  Format.formatter -> spilling_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Spilling_corner_case (loc,desc) ->
      let s = Format.asprintf "%s\n corner case: %s\n%s" loc desc (corner_case_message ()) in
      Format.pp_print_string f s
    | `Spilling_no_type_variable tv ->
      let s = Format.asprintf "%a@.Type \"%a\" not found (should not happen and be caught earlier)."
        Snippet.pp (Type_var.get_location tv)
        Type_var.pp tv in
      Format.pp_print_string f s
    | `Spilling_unsupported_pattern_matching loc ->
      let s = Format.asprintf "%a@.Invalid pattern matching.@Tuple patterns are not (yet) supported." Snippet.pp loc in
      Format.pp_print_string f s
    | `Spilling_unsupported_recursive_function (loc,var) ->
      let s = Format.asprintf "%a@.Invalid recursive function \"%a\".@.A recursive function can only have one argument."
        Snippet.pp loc
        Value_var.pp var in
      Format.pp_print_string f s
    | `Spilling_wrong_mini_c_value (expected , actual) ->
      let s = Format.asprintf "Invalid type.@.Expected \"%a\",@.but got \"%a\"."
        Ast_aggregated.PP.type_expression expected
        Mini_c.PP.value actual in
      Format.pp_print_string f s
    | `Spilling_bad_decompile bad ->
      let s = Format.asprintf "Cannot untranspile: %a"
        Mini_c.PP.value bad in
      Format.pp_print_string f s
    | `Spilling_could_not_parse_raw_michelson (loc, code) ->
      Format.fprintf f "@[<hv>%a@.Could not parse raw Michelson:@.\"%s\".@]"
        Snippet.pp loc
        code
    | `Spilling_raw_michelson_must_be_seq (loc, code) ->
      let open Tezos_micheline.Micheline in
      let open Tezos_micheline.Micheline_printer in
      Format.fprintf f "@[<hv>%a@.Raw Michelson must be seq (with curly braces {}), got: %a.@]"
        Snippet.pp loc
        print_expr
        (printable (fun s -> s) (strip_locations code))
  )

let error_json : spilling_error -> Error.t = fun e ->
  let open Error in
  match e with
  | `Spilling_corner_case (loc,desc) ->
    let message = Format.asprintf "%s\n corner case: %s\n%s" loc desc (corner_case_message ()) in
    let content = make_content ~message () in
    make ~stage ~content
  | `Spilling_no_type_variable tv ->
    let message = Format.asprintf "Type \"%a\" not found (should not happen and be caught earlier)." Type_var.pp tv in
    let location = Type_var.get_location tv in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Spilling_unsupported_pattern_matching location ->
    let message = Format.sprintf "Invalid pattern matching.@Tuple patterns are not (yet) supported." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Spilling_unsupported_recursive_function (location,var) ->
    let message = Format.asprintf "Invalid recursive function \"%a\".@.A recursive function can only have one argument." Value_var.pp var in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Spilling_wrong_mini_c_value (expected , actual) ->
    let message = Format.asprintf "Invalid type.@.Expected \"%a\",@.but got \"%a\"."
      Ast_aggregated.PP.type_expression expected
      Mini_c.PP.value actual in
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
    let message = Format.asprintf "Raw Michelson must be seq (with curly braces {}), got: %a."
      Micheline_printer.print_expr 
      (Micheline_printer.printable Fn.id (Micheline.strip_locations code)) in
    let content = make_content ~message ~location () in
    make ~stage ~content 

let error_jsonformat : spilling_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Spilling_corner_case (loc, desc) ->
    let content = `Assoc [
      ("location", `String loc);
      ("description", `String desc);
      ("message", `String (corner_case_message ()) ); ]
    in
    json_error ~stage ~content
  | `Spilling_no_type_variable tv ->
    let tv' = Format.asprintf "%a" Type_var.pp tv in
    let content = `Assoc [
      ("description", `String "type variables can't be transpiled");
      ("type_variable", `String tv'); ]
    in
    json_error ~stage ~content
  | `Spilling_unsupported_pattern_matching loc ->
    let loc' = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("location", `String loc');
      ("message", `String "unsupported tuple in pattern-matching"); ]
    in
    json_error ~stage ~content
  | `Spilling_unsupported_recursive_function (_,var) ->
    let var' = Format.asprintf "%a" Value_var.pp var in
    let content = `Assoc [
      ("message", `String "Recursive functions with only one variable are supported");
      ("value", `String var'); ]
    in
    json_error ~stage ~content
  | `Spilling_wrong_mini_c_value (expected , actual) ->
    let expected' = Format.asprintf "%a" Ast_aggregated.PP.type_expression expected in
    let actual' = Format.asprintf "%a" Mini_c.PP.value actual in
    let content = `Assoc [
      ("message", `String "illed type of intermediary value does not match what was expected");
      ("expected", `String expected');
      ("actual", `String actual'); ]
    in
    json_error ~stage ~content
  | `Spilling_bad_decompile bad ->
    let var' = Format.asprintf "%a" Mini_c.PP.value bad in
    let content = `Assoc [
      ("message", `String "untranspiling bad value");
      ("value", `String var'); ]
    in
    json_error ~stage ~content
  | `Spilling_could_not_parse_raw_michelson (loc, code) ->
    let loc' = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("location", `String loc');
      ("message", `String "Could not parse raw Michelson");
      ("code", `String code); ]
    in
    json_error ~stage ~content
  | `Spilling_raw_michelson_must_be_seq (loc, code) ->
    let loc' = Format.asprintf "%a" Location.pp loc in
    let open Tezos_micheline.Micheline in
    let open Tezos_micheline.Micheline_printer in
    let code =
      Format.asprintf "Raw Michelson must be seq (with curly braces {}), got: %a"
        print_expr
        (printable (fun s -> s) (strip_locations code)) in
    let content = `Assoc [
      ("location", `String loc');
      ("message", `String "Raw Michelson must be seq (with curly braces {})");
      ("code", `String code); ]
    in
    json_error ~stage ~content
