open Simple_utils.Display

module Raw = Cst.Jsligo
module Parsing  = Parsing.Jsligo
module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet

let stage = "abstracter"

type abs_error = [
  | `Concrete_jsligo_unknown_constant of string * Location.t
  | `Concrete_jsligo_unknown_constructor of string * Location.t
  | `Concrete_jsligo_untyped_recursive_fun of Region.t
  | `Concrete_jsligo_unsupported_pattern_type of Raw.pattern
  | `Concrete_jsligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_jsligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_jsligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_jsligo_recursion_on_non_function of Location.t
  | `Concrete_jsligo_funarg_tuple_type_mismatch of Region.t * Raw.pattern * Raw.type_expr
  | `Concrete_jsligo_not_in_switch_or_loop of Region.t
  | `Concrete_jsligo_statement_not_supported_at_toplevel of Raw.statement
  | `Concrete_jsligo_not_a_valid_parameter of Raw.expr
  | `Concrete_jsligo_rest_not_supported_here of Raw.property
  | `Concrete_jsligo_property_not_supported of Raw.property
  | `Concrete_jsligo_expected_an_expression of Raw.array_item
  | `Concrete_jsligo_new_not_supported of Raw.expr
  | `Concrete_jsligo_invalid_case of string * Raw.expr
  | `Concrete_jsligo_invalid_constructor of Raw.type_expr
  | `Concrete_jsligo_unsupported_match_pattern of Raw.expr
  | `Concrete_jsligo_unsupported_match_object_property of Raw.property
  | `Concrete_jsligo_expected_a_function of Raw.expr
  | `Concrete_jsligo_not_supported_assignment of Raw.expr
  | `Concrete_jsligo_array_rest_not_supported of Raw.array_item
  | `Concrete_jsligo_expected_a_variable of Region.t
  | `Concrete_jsligo_expected_a_field_name of Raw.selection
  | `Concrete_jsligo_expected_an_int of Raw.expr
  | `Concrete_jsligo_invalid_list_pattern_match of Raw.array_item list
  | `Concrete_jsligo_no_shared_fields of Region.t
  | `Concrete_jsligo_unexpected
  | `Concrete_jsligo_wrong_matchee_disc of Region.t
  | `Concrete_jsligo_case_break_disc of Region.t
  ] [@@deriving poly_constructor { prefix = "concrete_jsligo_" }]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_jsligo_unknown_constant (s,loc) ->
      Format.fprintf f
      "@[<hv>%a@.Unknown constant: %s"
        Snippet.pp loc s
    | `Concrete_jsligo_unknown_constructor (s,loc) ->
      Format.fprintf f
      "@[<hv>%a@.Unknown constructor in module: %s"
        Snippet.pp loc s
    | `Concrete_jsligo_untyped_recursive_fun reg ->
      Format.fprintf f
        "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to have a type annotation (for now). @]"
        Snippet.pp_lift reg
    | `Concrete_jsligo_unsupported_pattern_type pl ->
      Format.fprintf f
        "@[<hv>%a@.Invalid pattern matching.\
        @.  If this is pattern matching over Booleans, then \"true\" or \"false\" is expected.\
        @.  If this is pattern matching on a list, then one of the following is expected:\
        @.    * an empty list pattern \"[]\";\
        @.    * a cons list pattern \"[head, ...tail]\".\
        @.  If this is pattern matching over variants, then a constructor of a variant is expected.\
        @.\
        @.  Other forms of pattern matching are not (yet) supported. @]"
        Snippet.pp_lift ((fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl)
    | `Concrete_jsligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        Snippet.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_jsligo_recursion_on_non_function reg ->
      Format.fprintf f "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
        Snippet.pp reg
    | `Concrete_jsligo_michelson_type_wrong (texpr,name) ->
      Format.fprintf f
       "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type. @]"
          Snippet.pp_lift (Raw.type_expr_to_region texpr)
          name
    | `Concrete_jsligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string. @]"
        Snippet.pp loc
        name
    | `Concrete_jsligo_funarg_tuple_type_mismatch (region, pattern, texpr) -> (
      let p = Parsing.pretty_print_pattern pattern |> Buffer.contents in
      let t = Parsing.pretty_print_type_expr texpr |> Buffer.contents in
      Format.fprintf
        f
        "@[<hv>%a@.The tuple \"%s\" does not have the expected type \"%s\". @]"
        Snippet.pp_lift region
        p
        t
    )
    | `Concrete_jsligo_not_in_switch_or_loop reg -> (
        Format.fprintf f "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
          Snippet.pp_lift reg
    )
    | `Concrete_jsligo_statement_not_supported_at_toplevel s -> (
        Format.fprintf f "@[<hv>%a@.Statement not supported at toplevel.@.Only let, const, and type statements are supported at the toplevel. @]"
          Snippet.pp_lift (Raw.statement_to_region s)
    )
    | `Concrete_jsligo_not_a_valid_parameter p -> (
      Format.fprintf f "@[<hv>%a@.Not a valid function parameter. @]"
          Snippet.pp_lift (Raw.expr_to_region p)
    )
    | `Concrete_jsligo_rest_not_supported_here p -> (
      Format.fprintf f "@[<hv>%a@.Rest property not supported here. @]"
          Snippet.pp_lift (Raw.property_to_region p)
    )
    | `Concrete_jsligo_property_not_supported p -> (
      Format.fprintf f "@[<hv>%a@.This kind of property not supported here. @]"
          Snippet.pp_lift (Raw.property_to_region p)
    )
    | `Concrete_jsligo_expected_an_expression a -> (
      Format.fprintf f "@[<hv>%a@.Expected an expression. @]"
          Snippet.pp_lift (Raw.array_item_to_region a)
    )
    | `Concrete_jsligo_new_not_supported p -> (
      Format.fprintf f "@[<hv>%a@.'new' keyword not supported. @]"
          Snippet.pp_lift (Raw.expr_to_region p)
    )
    | `Concrete_jsligo_invalid_case (s, e) -> (
      Format.fprintf f "@[<hv>%a@.Invalid '%s' field value. An anonymous arrow function was expected, eg. `None: () => foo`.@]"
          Snippet.pp_lift (Raw.expr_to_region e) @@ s
    )
    | `Concrete_jsligo_invalid_constructor e -> (
      Format.fprintf f "@[<hv>%a@.Invalid constructor. Expected a constructor like: `[\"Foo\"]` or `[\"Foo\", int, string]`.@]"
          Snippet.pp_lift (Raw.type_expr_to_region e)
    )
    | `Concrete_jsligo_unsupported_match_pattern e -> (
      Format.fprintf f "@[<hv>%a@.Unsupported match pattern.@]"
          Snippet.pp_lift (Raw.expr_to_region e)
    )
    | `Concrete_jsligo_unsupported_match_object_property p -> (
      Format.fprintf f "@[<hv>%a@.Unsupported pattern match object property.@]"
          Snippet.pp_lift (Raw.property_to_region p)
    )
    | `Concrete_jsligo_expected_a_function e -> (
      Format.fprintf f "@[<hv>%a@.Expected a function.@]"
          Snippet.pp_lift (Raw.expr_to_region e)
    )
    | `Concrete_jsligo_not_supported_assignment e -> (
      Format.fprintf f "@[<hv>%a@.Not supported assignment.@]"
          Snippet.pp_lift (Raw.expr_to_region e)
    )
    | `Concrete_jsligo_array_rest_not_supported e -> (
      Format.fprintf f "@[<hv>%a@.Rest property not supported here.@]"
          Snippet.pp_lift (Raw.array_item_to_region e)
    )
    | `Concrete_jsligo_expected_a_variable reg -> (
      Format.fprintf f "@[<hv>%a@.Expected a variable.@]"
        Snippet.pp_lift reg
    )
    | `Concrete_jsligo_expected_a_field_name s -> (
      Format.fprintf f "@[<hv>%a@.Expected a field name.@]"
      Snippet.pp_lift (Raw.selection_to_region s)
    )
    | `Concrete_jsligo_expected_an_int e -> (
      Format.fprintf f "@[<hv>%a@.Expected an int.@]"
      Snippet.pp_lift (Raw.expr_to_region e)
    )
    | `Concrete_jsligo_invalid_list_pattern_match _l -> (
      Format.fprintf f "@[<hv>Invalid list pattern matching.@]"
    )
    | `Concrete_jsligo_no_shared_fields r -> (
      Format.fprintf f "@[<hv>%aAll the objects are expected to have a shared field with an unique value.@]"
      Snippet.pp_lift r
    )
    | `Concrete_jsligo_unexpected -> 
      Format.fprintf f "@[<hv>Unexpected error.@]"
    | `Concrete_jsligo_wrong_matchee_disc r -> (
      Format.fprintf f "@[<hv>%aExpected a record field. Eg. `field.kind` or `some.other.id`, but not `foo`. @]"
      Snippet.pp_lift r
    )
    | `Concrete_jsligo_case_break_disc r ->
      Format.fprintf f "@[<hv>%aA discriminated union case needs to end with a break. A fallthrough to another case is currently not supported. @]"
      Snippet.pp_lift r
  )


let error_jsonformat : abs_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Concrete_jsligo_unknown_constant (s,loc) ->
    let message = `String ("Unknow constant: " ^ s) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Concrete_jsligo_unknown_constructor (s,loc) ->
    let message = `String ("Unknow constructor in module: " ^ s) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Concrete_jsligo_untyped_recursive_fun reg ->
    let message = `String "Untyped recursive functions are not supported yet" in
    let loc = Location.lift reg in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unsupported_pattern_type pl ->
    let loc = Location.lift ((fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl) in
    let message = `String "Currently, only booleans, lists, options, and constructors are supported in patterns" in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unsupported_string_singleton te ->
    let message = `String "Unsupported singleton string type" in
    let loc = Location.lift (Raw.type_expr_to_region te) in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_recursion_on_non_function loc ->
    let message = Format.asprintf "Only functions can be recursive." in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc) ] in
    json_error ~stage ~content
  | `Concrete_jsligo_michelson_type_wrong (texpr,name) ->
    let message = Format.asprintf "Argument of %s must be a string singleton" name in
    let loc = Location.lift (Raw.type_expr_to_region texpr) in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc); ] in
    json_error ~stage ~content
  | `Concrete_jsligo_michelson_type_wrong_arity (loc,name) ->
    let message = Format.asprintf "%s does not have the right number of argument" name in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc); ] in
    json_error ~stage ~content
  | `Concrete_jsligo_funarg_tuple_type_mismatch (r, _, _) ->
    let message = Format.asprintf "The tuple does not have the expected type." in
    let loc = Location.lift r in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Concrete_jsligo_not_in_switch_or_loop reg ->
    let message = `String "Not in switch or loop." in
    let loc = Location.lift reg in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_statement_not_supported_at_toplevel statement ->
    let message = `String "Statement not supported at toplevel." in
    let loc = Location.lift (Raw.statement_to_region statement) in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_not_a_valid_parameter expr ->
    let message = `String "Not a valid function parameter." in
    let loc = Location.lift (Raw.expr_to_region expr) in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_rest_not_supported_here p ->
    let message = `String "Rest property not supported here." in
    let loc = Location.lift (Raw.property_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_property_not_supported p ->
    let message = `String "This kind of property not supported here." in
    let loc = Location.lift (Raw.property_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_an_expression p ->
    let message = `String "Expected an expression." in
    let loc = Location.lift (Raw.array_item_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_new_not_supported p ->
    let message = `String "'new' keyword not supported." in
    let loc = Location.lift (Raw.expr_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_invalid_case (_, e) ->
    let message = `String "Invalid case." in
    let loc = Location.lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_invalid_constructor e ->
    let message = `String "Invalid constructor." in
    let loc = Location.lift (Raw.type_expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unsupported_match_pattern e ->
    let message = `String "Unsupported match pattern." in
    let loc = Location.lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unsupported_match_object_property p ->
    let message = `String "Unsupported pattern match object property." in
    let loc = Location.lift (Raw.property_to_region p) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_a_function e ->
    let message = `String "Expected a function." in
    let loc = Location.lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_not_supported_assignment e ->
    let message = `String "Not supported asignment." in
    let loc = Location.lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_array_rest_not_supported e ->
    let message = `String "Rest property not supported here." in
    let loc = Location.lift (Raw.array_item_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_a_variable reg ->
    let message = `String "Expected a variable." in
    let loc = Location.lift reg in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_a_field_name s ->
    let message = `String "Expected a field name." in
    let loc = Location.lift (Raw.selection_to_region s) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_an_int e ->
    let message = `String "Expected an int." in
    let loc = Location.lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_invalid_list_pattern_match _l ->
    let message = `String "Expected an int." in
    (* let loc = Location.lift (Raw.expr_to_region e) in *)
    let content = `Assoc [
      ("message", message);
      (* ("location", Location.to_yojson loc); *)
      ] in
    json_error ~stage ~content
  | `Concrete_jsligo_no_shared_fields reg ->
    let message = `String "No shared field." in
    let loc = Location.lift reg in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unexpected ->
    let message = `String "Unexpected error." in
    (* let loc = Location.lift (Raw.expr_to_region e) in *)
    let content = `Assoc [
      ("message", message);
      (* ("location", Location.to_yojson loc); *)
      ] in
    json_error ~stage ~content
  | `Concrete_jsligo_wrong_matchee_disc reg ->
    let message = `String "Wrong matchee." in
    let loc = Location.lift reg in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_case_break_disc reg ->
    let message = `String "Discriminatory union case needs to end with break." in
    let loc = Location.lift reg in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content