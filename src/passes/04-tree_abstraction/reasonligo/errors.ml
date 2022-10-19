open Simple_utils.Display

module Raw = Cst.Reasonligo
module Parsing = Parsing.Reasonligo
module Region  = Simple_utils.Region
module Snippet = Simple_utils.Snippet
module Location = Simple_utils.Location

let stage = "abstracter"

type abs_error = [
  | `Concrete_reasonligo_unknown_constant of string * Location.t
  | `Concrete_reasonligo_untyped_recursive_fun of Location.t
  | `Concrete_reasonligo_unsupported_pattern_type of Raw.pattern
  | `Concrete_reasonligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_reasonligo_michelson_type_wrong of Location.t * string
  | `Concrete_reasonligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_reasonligo_recursion_on_non_function of Location.t
  | `Concrete_reasonligo_funarg_tuple_type_mismatch of Region.t * Raw.pattern * Raw.type_expr
  | `Concrete_reasonligo_expected_access_to_variable of Region.t

  ] [@@deriving poly_constructor { prefix = "concrete_reasonligo_" }]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_reasonligo_expected_access_to_variable reg ->
      Format.fprintf f
      "@[<hv>%a@.Expected access to a variable.@]"
        Snippet.pp_lift reg
    | `Concrete_reasonligo_unknown_constant (s,loc) ->
      Format.fprintf f
      "@[<hv>%a@.Unknown constant: %s"
        Snippet.pp loc s
    | `Concrete_reasonligo_untyped_recursive_fun loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to have a type annotation (for now). @]"
        Snippet.pp loc
    | `Concrete_reasonligo_unsupported_pattern_type pl ->
      Format.fprintf f
        "@[<hv>%a@.Invalid pattern matching.\
        @.Can't match on values. @]"
        Snippet.pp_lift ((fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl)
    | `Concrete_reasonligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        Snippet.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_reasonligo_recursion_on_non_function reg ->
      Format.fprintf f "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
        Snippet.pp reg
    | `Concrete_reasonligo_michelson_type_wrong (loc,name) ->
      Format.fprintf f
       "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type. @]"
          Snippet.pp loc
          name
    | `Concrete_reasonligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string. @]"
        Snippet.pp loc
        name
    | `Concrete_reasonligo_funarg_tuple_type_mismatch (region, pattern, texpr) -> (
      let p = Parsing.pretty_print_pattern pattern |> Buffer.contents in
      let t = Parsing.pretty_print_type_expr texpr |> Buffer.contents in
      Format.fprintf
        f
        "@[<hv>%a@.The tuple \"%s\" does not have the expected type \"%s\". @]"
        Snippet.pp_lift region
        p
        t
    )
  )

let error_json : abs_error -> Ligo_prim.Error.t =
  fun e ->
    let open Ligo_prim.Error in
    match e with
    | `Concrete_reasonligo_expected_access_to_variable reg ->
      let message = "Expected access to a variable." in
      let location = Location.File reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_reasonligo_unknown_constant (s,location) ->
      let message = Format.sprintf "Unknown constant: %s" s in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_reasonligo_untyped_recursive_fun location ->
      let message = Format.sprintf "Invalid function declaration.@.Recursive functions are required to have a type annotation (for now)." in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_reasonligo_unsupported_pattern_type pl ->
      let message = "Invalid pattern matching.@.Can't match on values." in
      let reg = Raw.pattern_to_region pl in
      let location = Location.File reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_reasonligo_unsupported_string_singleton te ->
      let message = Format.sprintf "Invalid type. @.It's not possible to assign a string to a type." in
      let location = Location.File (Raw.type_expr_to_region te) in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_reasonligo_recursion_on_non_function location ->
      let message = "Invalid let declaration.@.Only functions can be recursive." in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_reasonligo_michelson_type_wrong (location,name) ->
      let message = Format.sprintf "Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type." name in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_reasonligo_michelson_type_wrong_arity (location,name) ->
      let message = Format.sprintf "Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string." name in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_reasonligo_funarg_tuple_type_mismatch (region, pattern, texpr) ->
      let p = Parsing.pretty_print_pattern pattern |> Buffer.contents in
      let t = Parsing.pretty_print_type_expr texpr |> Buffer.contents in
      let message = Format.sprintf "The tuple \"%s\" does not have the expected type \"%s\"." p t in
      let location = Location.File region in
      let content = make_content ~message ~location () in
      make ~stage ~content

let error_jsonformat : abs_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Concrete_reasonligo_expected_access_to_variable reg ->
    let message = `String "Expected access to a variable" in
    let loc = Format.asprintf "%a" Location.pp_lift reg in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_reasonligo_unknown_constant (s,loc) ->
    let message = `String ("Unknow constant: " ^ s) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Concrete_reasonligo_untyped_recursive_fun loc ->
    let message = `String "Untyped recursive functions are not supported yet" in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_reasonligo_unsupported_pattern_type pl ->
    let loc = Location.lift ((fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl) in
    let message = `String "Currently, only booleans, lists, options, and constructors are supported in patterns" in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_reasonligo_unsupported_string_singleton te ->
    let message = `String "Unsupported singleton string type" in
    let loc = Location.lift (Raw.type_expr_to_region te) in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_reasonligo_recursion_on_non_function loc ->
    let message = Format.asprintf "Only functions can be recursive." in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc) ] in
    json_error ~stage ~content
  | `Concrete_reasonligo_michelson_type_wrong (loc,name) ->
    let message = Format.asprintf "Argument %s must be a string singleton" name in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc); ] in
    json_error ~stage ~content
  | `Concrete_reasonligo_michelson_type_wrong_arity (loc,name) ->
    let message = Format.asprintf "%s does not have the right number of argument" name in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc); ] in
    json_error ~stage ~content
  | `Concrete_reasonligo_funarg_tuple_type_mismatch (r, _, _) ->
    let message = Format.asprintf "The tuple does not have the expected type." in
    let loc = Location.lift r in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
