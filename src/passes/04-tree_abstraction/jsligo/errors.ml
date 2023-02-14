open Simple_utils.Display
module Raw = Cst.Jsligo
module Parsing = Parsing.Jsligo
module Region = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet = Simple_utils.Snippet

let stage = "abstracter"

type abs_error =
  [ `Concrete_jsligo_unknown_constant of string * Location.t
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
  | `Concrete_jsligo_expected_an_int_or_string of Raw.expr
  | `Concrete_jsligo_invalid_list_pattern_match of Raw.array_item list
  | `Concrete_jsligo_no_shared_fields of Region.t
  | `Concrete_jsligo_unexpected
  | `Concrete_jsligo_wrong_matchee_disc of Region.t
  | `Concrete_jsligo_case_break_disc of Region.t
  | `Concrete_jsligo_not_implemented of Region.t
  ]
[@@deriving poly_constructor { prefix = "concrete_jsligo_" }]

let error_ppformat
    :  display_format:string display_format -> no_colour:bool -> Format.formatter
    -> abs_error -> unit
  =
 fun ~display_format ~no_colour f a ->
  let snippet_pp = Snippet.pp ~no_colour in
  let snippet_pp_lift = Snippet.pp_lift ~no_colour in
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Concrete_jsligo_unknown_constant (s, loc) ->
      Format.fprintf f "@[<hv>%a@.Unknown constant: %s" snippet_pp loc s
    | `Concrete_jsligo_unknown_constructor (s, loc) ->
      Format.fprintf f "@[<hv>%a@.Unknown constructor in module: %s" snippet_pp loc s
    | `Concrete_jsligo_untyped_recursive_fun reg ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to \
         have a type annotation (for now). @]"
        snippet_pp_lift
        reg
    | `Concrete_jsligo_unsupported_pattern_type pl ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid pattern matching.@.  If this is pattern matching over \
         Booleans, then \"true\" or \"false\" is expected.@.  If this is pattern \
         matching on a list, then one of the following is expected:@.    * an empty list \
         pattern \"[]\";@.    * a cons list pattern \"[head, ...tail]\".@.  If this is \
         pattern matching over variants, then a constructor of a variant is \
         expected.@.@.  Other forms of pattern matching are not (yet) supported. @]"
        snippet_pp_lift
        ((fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl)
    | `Concrete_jsligo_unsupported_string_singleton te ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        snippet_pp_lift
        (Raw.type_expr_to_region te)
    | `Concrete_jsligo_recursion_on_non_function reg ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
        snippet_pp
        reg
    | `Concrete_jsligo_michelson_type_wrong (texpr, name) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a \
         string, is expected for the preceding type. @]"
        snippet_pp_lift
        (Raw.type_expr_to_region texpr)
        name
    | `Concrete_jsligo_michelson_type_wrong_arity (loc, name) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is \
         expected, where each odd item is a type annotated by the following string. @]"
        snippet_pp
        loc
        name
    | `Concrete_jsligo_funarg_tuple_type_mismatch (region, pattern, texpr) ->
      let p = Parsing.pretty_print_pattern pattern |> Buffer.contents in
      let t = Parsing.pretty_print_type_expr texpr |> Buffer.contents in
      Format.fprintf
        f
        "@[<hv>%a@.The tuple \"%s\" does not have the expected type \"%s\". @]"
        snippet_pp_lift
        region
        p
        t
    | `Concrete_jsligo_not_in_switch_or_loop reg ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
        snippet_pp_lift
        reg
    | `Concrete_jsligo_statement_not_supported_at_toplevel s ->
      Format.fprintf
        f
        "@[<hv>%a@.Statement not supported at toplevel.@.Only let, const, and type \
         statements are supported at the toplevel. @]"
        snippet_pp_lift
        (Raw.statement_to_region s)
    | `Concrete_jsligo_not_a_valid_parameter p ->
      Format.fprintf
        f
        "@[<hv>%a@.Not a valid function parameter. @]"
        snippet_pp_lift
        (Raw.expr_to_region p)
    | `Concrete_jsligo_rest_not_supported_here p ->
      Format.fprintf
        f
        "@[<hv>%a@.Rest property not supported here. @]"
        snippet_pp_lift
        (Raw.property_to_region p)
    | `Concrete_jsligo_property_not_supported p ->
      Format.fprintf
        f
        "@[<hv>%a@.This kind of property not supported here. @]"
        snippet_pp_lift
        (Raw.property_to_region p)
    | `Concrete_jsligo_expected_an_expression a ->
      Format.fprintf
        f
        "@[<hv>%a@.Expected an expression. @]"
        snippet_pp_lift
        (Raw.array_item_to_region a)
    | `Concrete_jsligo_new_not_supported p ->
      Format.fprintf
        f
        "@[<hv>%a@.'new' keyword not supported. @]"
        snippet_pp_lift
        (Raw.expr_to_region p)
    | `Concrete_jsligo_invalid_case (s, e) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid '%s' field value. An anonymous arrow function was expected, \
         eg. `None: () => foo`.@]"
        snippet_pp_lift
        (Raw.expr_to_region e)
      @@ s
    | `Concrete_jsligo_invalid_constructor e ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid constructor. Expected a constructor like: `[\"Foo\"]` or \
         `[\"Foo\", int, string]`.@]"
        snippet_pp_lift
        (Raw.type_expr_to_region e)
    | `Concrete_jsligo_unsupported_match_pattern e ->
      Format.fprintf
        f
        "@[<hv>%a@.Unsupported match pattern.@]"
        snippet_pp_lift
        (Raw.expr_to_region e)
    | `Concrete_jsligo_unsupported_match_object_property p ->
      Format.fprintf
        f
        "@[<hv>%a@.Unsupported pattern match object property.@]"
        snippet_pp_lift
        (Raw.property_to_region p)
    | `Concrete_jsligo_expected_a_function e ->
      Format.fprintf
        f
        "@[<hv>%a@.Expected a function.@]"
        snippet_pp_lift
        (Raw.expr_to_region e)
    | `Concrete_jsligo_not_supported_assignment e ->
      Format.fprintf
        f
        "@[<hv>%a@.Not supported assignment.@]"
        snippet_pp_lift
        (Raw.expr_to_region e)
    | `Concrete_jsligo_array_rest_not_supported e ->
      Format.fprintf
        f
        "@[<hv>%a@.Rest property not supported here.@]"
        snippet_pp_lift
        (Raw.array_item_to_region e)
    | `Concrete_jsligo_expected_a_variable reg ->
      Format.fprintf f "@[<hv>%a@.Expected a variable.@]" snippet_pp_lift reg
    | `Concrete_jsligo_expected_a_field_name s ->
      Format.fprintf
        f
        "@[<hv>%a@.Expected a field name.@]"
        snippet_pp_lift
        (Raw.selection_to_region s)
    | `Concrete_jsligo_expected_an_int_or_string e ->
      Format.fprintf
        f
        "@[<hv>%a@.Expected an int or a string.@]"
        snippet_pp_lift
        (Raw.expr_to_region e)
    | `Concrete_jsligo_invalid_list_pattern_match _l ->
      Format.fprintf f "@[<hv>Invalid list pattern matching.@]"
    | `Concrete_jsligo_no_shared_fields r ->
      Format.fprintf
        f
        "@[<hv>%aAll the objects are expected to have a shared field with an unique \
         value.@]"
        snippet_pp_lift
        r
    | `Concrete_jsligo_unexpected -> Format.fprintf f "@[<hv>Unexpected error.@]"
    | `Concrete_jsligo_wrong_matchee_disc r ->
      Format.fprintf
        f
        "@[<hv>%aExpected a record field. For example: `field.kind`. @]"
        snippet_pp_lift
        r
    | `Concrete_jsligo_case_break_disc r ->
      Format.fprintf
        f
        "@[<hv>%aA discriminated union case needs to end with a `break` or `return` \
         statement. A fallthrough to another case is currently not supported. @]"
        snippet_pp_lift
        r
    | `Concrete_jsligo_not_implemented r ->
      Format.fprintf f "@[<hv>%aThis has not been implemented yet. @]" snippet_pp_lift r)


let error_json : abs_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Concrete_jsligo_unknown_constant (s, location) ->
    let message = Format.sprintf "Unknown constant: %s." s in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_unknown_constructor (s, location) ->
    let message = Format.sprintf "Unknown constructor in module: %s." s in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_untyped_recursive_fun reg ->
    let message =
      "Invalid function declaration.@.Recursive functions are required to have a type \
       annotation (for now)."
    in
    let location = Location.lift reg in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_unsupported_pattern_type pl ->
    let message =
      Format.sprintf
        "Invalid pattern matching.@.  If this is pattern matching over Booleans, then \
         \"true\" or \"false\" is expected.@.  If this is pattern matching on a list, \
         then one of the following is expected:@.    * an empty list pattern \
         \"[]\";@.    * a cons list pattern \"[head, ...tail]\".@.  If this is pattern \
         matching over variants, then a constructor of a variant is expected.@.@.  Other \
         forms of pattern matching are not (yet) supported."
    in
    let location = Location.lift (Raw.pattern_to_region pl) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_unsupported_string_singleton te ->
    let message = "Invalid type. @.It's not possible to assign a string to a type." in
    let location = Location.lift (Raw.type_expr_to_region te) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_recursion_on_non_function location ->
    let message = "Invalid let declaration.@.Only functions can be recursive." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_michelson_type_wrong (texpr, name) ->
    let message =
      Format.sprintf
        "Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is \
         expected for the preceding type."
        name
    in
    let location = Location.lift (Raw.type_expr_to_region texpr) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_michelson_type_wrong_arity (location, name) ->
    let message =
      Format.sprintf
        "Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where \
         each odd item is a type annotated by the following string."
        name
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_funarg_tuple_type_mismatch (region, pattern, texpr) ->
    let p = Parsing.pretty_print_pattern pattern |> Buffer.contents in
    let t = Parsing.pretty_print_type_expr texpr |> Buffer.contents in
    let message =
      Format.sprintf "The tuple \"%s\" does not have the expected type \"%s\"." p t
    in
    let location = Location.lift region in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_not_in_switch_or_loop reg ->
    let message =
      Format.sprintf "Invalid let declaration.@.Only functions can be recursive."
    in
    let location = Location.lift reg in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_statement_not_supported_at_toplevel s ->
    let message =
      Format.sprintf
        "Statement not supported at toplevel.@.Only let, const, and type statements are \
         supported at the toplevel."
    in
    let location = Location.lift (Raw.statement_to_region s) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_not_a_valid_parameter p ->
    let message = Format.sprintf "Not a valid function parameter." in
    let location = Location.lift (Raw.expr_to_region p) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_rest_not_supported_here p ->
    let message = Format.sprintf "Rest property not supported here." in
    let location = Location.lift (Raw.property_to_region p) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_property_not_supported p ->
    let message = Format.sprintf "This kind of property not supported here." in
    let location = Location.lift (Raw.property_to_region p) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_expected_an_expression a ->
    let message = Format.sprintf "Expected an expression." in
    let location = Location.lift (Raw.array_item_to_region a) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_new_not_supported p ->
    let message = Format.sprintf "'new' keyword not supported." in
    let location = Location.lift (Raw.expr_to_region p) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_invalid_case (s, e) ->
    let message =
      Format.sprintf
        "Invalid '%s' field value. An anonymous arrow function was expected, eg. `None: \
         () => foo`."
        s
    in
    let location = Location.lift (Raw.expr_to_region e) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_invalid_constructor e ->
    let message =
      Format.sprintf
        "Invalid constructor. Expected a constructor like: `[\"Foo\"]` or `[\"Foo\", \
         int, string]`."
    in
    let location = Location.lift (Raw.type_expr_to_region e) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_unsupported_match_pattern e ->
    let message = Format.sprintf "Unsupported match pattern." in
    let location = Location.lift (Raw.expr_to_region e) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_unsupported_match_object_property p ->
    let message = Format.sprintf "Unsupported pattern match object property." in
    let location = Location.lift (Raw.property_to_region p) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_expected_a_function e ->
    let message = Format.sprintf "Expected a function." in
    let location = Location.lift (Raw.expr_to_region e) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_not_supported_assignment e ->
    let message = Format.sprintf "Not supported assignment." in
    let location = Location.lift (Raw.expr_to_region e) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_array_rest_not_supported e ->
    let message = Format.sprintf "Rest property not supported here." in
    let location = Location.lift (Raw.array_item_to_region e) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_expected_a_variable reg ->
    let message = Format.sprintf "Expected a variable." in
    let location = Location.lift reg in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_expected_a_field_name s ->
    let message = Format.sprintf "Expected a field name." in
    let location = Location.lift (Raw.selection_to_region s) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_expected_an_int_or_string e ->
    let message = Format.sprintf "Expected an int or a string." in
    let location = Location.lift (Raw.expr_to_region e) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_invalid_list_pattern_match _l ->
    let message = Format.sprintf "Invalid list pattern matching." in
    let content = make_content ~message () in
    make ~stage ~content
  | `Concrete_jsligo_no_shared_fields r ->
    let message =
      Format.sprintf
        "All the objects are expected to have a shared field with an unique value."
    in
    let location = Location.lift r in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_unexpected ->
    let message = Format.sprintf "Unexpected error." in
    let content = make_content ~message () in
    make ~stage ~content
  | `Concrete_jsligo_wrong_matchee_disc r ->
    let message = Format.sprintf "Expected a record field. For example: `field.kind`." in
    let location = Location.lift r in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_case_break_disc r ->
    let message =
      Format.sprintf
        "A discriminated union case needs to end with a `break` or `return` statement. A \
         fallthrough to another case is currently not supported."
    in
    let location = Location.lift r in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_jsligo_not_implemented r ->
    let message = Format.sprintf "This has not been implemented yet." in
    let location = Location.lift r in
    let content = make_content ~message ~location () in
    make ~stage ~content
