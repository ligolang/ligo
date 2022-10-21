open Simple_utils.Display

module Raw      = Cst.Pascaligo
module Region   = Simple_utils.Region
module Snippet  = Simple_utils.Snippet
module Location = Simple_utils.Location

let stage = "abstracter"

type abs_error = [
  | `Concrete_pascaligo_unknown_constant of string * Location.t
  | `Concrete_pascaligo_unsupported_pattern_type of Raw.pattern
  | `Concrete_pascaligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_pascaligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_pascaligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_pascaligo_untyped_recursive_fun of Location.t
  | `Concrete_pascaligo_block_start_with_attribute of Raw.block Region.reg
  | `Concrete_pascaligo_unsupported_top_level_destructuring of Region.t
  | `Concrete_pascaligo_unsupported_type_ann_on_patterns of Region.t
  | `Concrete_pascaligo_ignored_attribute of Location.t
  | `Concrete_pascaligo_expected_variable of Location.t
  | `Concrete_pascaligo_expected_field_name of Region.t
  | `Concrete_pascaligo_expected_field_or_access of Region.t
  | `Concrete_pascaligo_wrong_functional_lens of Region.t
  | `Concrete_pascaligo_unexpected_wildcard of Region.t
  | `Concrete_pascaligo_wrong_functional_updator of Region.t
  | `Concrete_pascaligo_unsuported_pattern_in_function of Region.t
  | `Concrete_pascaligo_wrong_lvalue of Region.t
  ] [@@deriving poly_constructor { prefix = "concrete_pascaligo_" }]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_pascaligo_wrong_lvalue reg ->
      Format.fprintf f
        "@[<hv>%a@.Effectful updates must be performed on identified objects that are not accessed through a module@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_unsupported_type_ann_on_patterns reg ->
      Format.fprintf f
        "@[<hv>%a@.Type annotations on this kind of patterns are not supported yet@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_unsuported_pattern_in_function reg ->
      Format.fprintf f
        "@[<hv>%a@.These kind of patterns are not supported in function parameters@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_unexpected_wildcard reg ->
      Format.fprintf f
        "@[<hv>%a@.Wildcards ('_') are not supported yet@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_expected_field_name reg ->
      Format.fprintf f
        "@[<hv>%a@.Expected a field name@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_expected_field_or_access reg ->
      Format.fprintf f
        "@[<hv>%a@.Expected a field name or an accessor@]"
        Snippet.pp_lift reg
    | `Concrete_pascaligo_wrong_functional_lens reg ->
      Format.fprintf f
        "@[<hv>%a@.Functional lenses can't be used in record expressions@]"
        Snippet.pp_lift reg
    | `Concrete_pascaligo_ignored_attribute loc ->
      Format.fprintf f
        "@[<hv>%a@.Attribute being ignored@]"
        Snippet.pp loc
    | `Concrete_pascaligo_expected_variable loc ->
      Format.fprintf f
        "@[<hv>%a@.Expected a declaration name@]"
        Snippet.pp loc
    | `Concrete_pascaligo_wrong_functional_updator reg ->
      Format.fprintf f
        "@[<hv>%a@.Functional update only work on records@]"
        Snippet.pp_lift reg
    | `Concrete_pascaligo_unknown_constant (s,loc) ->
      Format.fprintf f
      "@[<hv>%a@.Unknown constant: %s"
        Snippet.pp loc s
    | `Concrete_pascaligo_unsupported_pattern_type pl ->
      Format.fprintf f
        "@[<hv>%a@.Invalid case pattern.\
        @.Can't match on values. @]"
        Snippet.pp_lift @@ Raw.pattern_to_region pl
    | `Concrete_pascaligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        Snippet.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_pascaligo_michelson_type_wrong (texpr,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type. @]"
          Snippet.pp_lift (Raw.type_expr_to_region texpr)
          name
    | `Concrete_pascaligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string. @]"
        Snippet.pp loc
        name
    | `Concrete_pascaligo_untyped_recursive_fun loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to have a type annotation (for now). @]"
        Snippet.pp loc
    | `Concrete_pascaligo_block_start_with_attribute block ->
      Format.fprintf f
        "@[<hv>%a@.Invalid attribute declaration.@.Attributes have to follow the declaration it is attached to. @]"
        Snippet.pp_lift @@ block.region
    | `Concrete_pascaligo_unsupported_top_level_destructuring loc ->
      Format.fprintf f
        "@[<hv>%a@.Unsupported destructuring at top-level. @]"
        Snippet.pp_lift @@ loc
  )

let error_json : abs_error -> Simple_utils.Error.t =
  fun e ->
    let open Simple_utils.Error in
    match e with
    | `Concrete_pascaligo_wrong_lvalue reg ->
      let message = "Effectful updates must be performed on identified objects that are not accessed through a module." in
      let location = Location.lift reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_unsupported_type_ann_on_patterns reg ->
      let message = "Type annotations on this kind of patterns are not supported yet." in
      let location = Location.lift reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_unsuported_pattern_in_function reg ->
      let message = "These kind of patterns are not supported in function parameters." in
      let location = Location.lift reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_unexpected_wildcard reg ->
      let message = "Wildcards ('_') are not supported yet." in
      let location = Location.lift reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_expected_field_name reg ->
      let message = "Expected a field name." in
      let location = Location.lift reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_expected_field_or_access reg ->
      let message = "Expected a field name or an accessor." in
      let location = Location.lift reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_wrong_functional_lens reg ->
      let message = "Functional lenses can't be used in record expressions." in
      let location = Location.lift reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_ignored_attribute location ->
      let message = "Attribute being ignored." in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_expected_variable location ->
      let message = "Expected a declaration name." in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_wrong_functional_updator reg ->
      let message = "Functional update only work on records." in
      let location = Location.lift reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_unknown_constant (s,location) ->
      let message = Format.sprintf "Unknown constant: %s." s in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_unsupported_pattern_type pl ->
      let message = Format.sprintf "Invalid case pattern.@.Can't match on values." in
      let location = Location.lift (Raw.pattern_to_region pl) in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_unsupported_string_singleton te ->
      let message = Format.sprintf "Invalid type. @.It's not possible to assign a string to a type." in
      let location = Location.lift (Raw.type_expr_to_region te) in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_michelson_type_wrong (texpr,name) ->
      let message = Format.sprintf "Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type." name in
      let location = Location.lift (Raw.type_expr_to_region texpr) in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_michelson_type_wrong_arity (location,name) ->
      let message = Format.sprintf "Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string." name in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_untyped_recursive_fun location ->
      let message = "Invalid function declaration.@.Recursive functions are required to have a type annotation (for now)." in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_block_start_with_attribute block ->
      let message = "Invalid attribute declaration.@.Attributes have to follow the declaration it is attached to." in
      let location = Location.lift block.region in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Concrete_pascaligo_unsupported_top_level_destructuring reg ->
      let message = "Unsupported destructuring at top-level." in
      let location = Location.lift reg in
      let content = make_content ~message ~location () in
      make ~stage ~content
