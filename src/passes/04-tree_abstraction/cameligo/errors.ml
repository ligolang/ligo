open Simple_utils.Display
module Raw = Cst.Cameligo
module Parsing = Parsing.Cameligo
module Region = Simple_utils.Region
module Snippet = Simple_utils.Snippet
module Location = Simple_utils.Location

let stage = "abstracter"

type abs_error =
  [ `Concrete_cameligo_untyped_recursive_fun of Region.t
  | `Concrete_cameligo_unknown_constant of string * Location.t
  | `Concrete_cameligo_unsupported_pattern_type of Raw.pattern list
  | `Concrete_cameligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_cameligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_cameligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_cameligo_recursion_on_non_function of Location.t
  | `Concrete_cameligo_missing_funarg_annotation of Raw.variable
  | `Concrete_cameligo_funarg_tuple_type_mismatch of
    Region.t * Raw.pattern * Raw.type_expr
  | `Concrete_cameligo_type_params_not_annotated of Region.t
  | `Concrete_cameligo_expected_access_to_variable of Region.t
  ]
[@@deriving poly_constructor { prefix = "concrete_cameligo_" }]

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
    | `Concrete_cameligo_expected_access_to_variable reg ->
      Format.fprintf f "@[<hv>%a@.Expected access to a variable.@]" snippet_pp_lift reg
    | `Concrete_cameligo_untyped_recursive_fun reg ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to \
         have a type annotation (for now). @]"
        snippet_pp_lift
        reg
    | `Concrete_cameligo_unknown_constant (s, reg) ->
      Format.fprintf f "@[<hv>%a@.Unknown constant: %s" snippet_pp reg s
    | `Concrete_cameligo_unsupported_pattern_type pl ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid pattern.@.Can't match on values. @]"
        snippet_pp_lift
        (List.fold_left
           ~f:(fun a p -> Region.cover a (Raw.pattern_to_region p))
           ~init:Region.ghost
           pl)
    | `Concrete_cameligo_unsupported_string_singleton te ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        snippet_pp_lift
        (Raw.type_expr_to_region te)
    | `Concrete_cameligo_recursion_on_non_function reg ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
        snippet_pp
        reg
    | `Concrete_cameligo_michelson_type_wrong (texpr, name) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a \
         string, is expected for the preceding type. @]"
        snippet_pp_lift
        (Raw.type_expr_to_region texpr)
        name
    | `Concrete_cameligo_michelson_type_wrong_arity (loc, name) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is \
         expected, where each odd item is a type annotated by the following string. @]"
        snippet_pp
        loc
        name
    | `Concrete_cameligo_missing_funarg_annotation v ->
      Format.fprintf
        f
        "@[<hv>%a@.Missing a type annotation for argument \"%s\". @]"
        snippet_pp_lift
        v.region
        v.value
    | `Concrete_cameligo_funarg_tuple_type_mismatch (region, pattern, texpr) ->
      let p = Parsing.pretty_print_pattern Parsing.Pretty.default_environment pattern |> Buffer.contents in
      let t = Parsing.pretty_print_type_expr Parsing.Pretty.default_environment texpr |> Buffer.contents in
      Format.fprintf
        f
        "@[<hv>%a@.The tuple \"%s\" does not have the expected type \"%s\". @]"
        snippet_pp_lift
        region
        p
        t
    | `Concrete_cameligo_type_params_not_annotated reg ->
      Format.fprintf
        f
        "@[<hv>%a@.Functions with type parameters need to be annotated. @]"
        snippet_pp_lift
        reg)


let error_json : abs_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Concrete_cameligo_expected_access_to_variable reg ->
    let message = "Expected access to a variable." in
    let location = Location.lift reg in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_untyped_recursive_fun reg ->
    let message =
      "Invalid function declaration.@.Recursive functions are required to have a type \
       annotation (for now)."
    in
    let location = Location.lift reg in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_unknown_constant (s, location) ->
    let message = Format.sprintf "Unknown constant: %s" s in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_unsupported_pattern_type pl ->
    let message = "Invalid pattern.@.Can't match on values." in
    let reg =
      List.fold_left
        ~f:(fun a p -> Region.cover a (Raw.pattern_to_region p))
        ~init:Region.ghost
        pl
    in
    let location = Location.lift reg in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_unsupported_string_singleton te ->
    let message = "Invalid type. @.It's not possible to assign a string to a type." in
    let location = Location.lift (Raw.type_expr_to_region te) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_recursion_on_non_function location ->
    let message = "Invalid let declaration.@.Only functions can be recursive." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_michelson_type_wrong (texpr, name) ->
    let message =
      Format.sprintf
        "Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is \
         expected for the preceding type."
        name
    in
    let location = Location.lift (Raw.type_expr_to_region texpr) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_michelson_type_wrong_arity (location, name) ->
    let message =
      Format.sprintf
        "Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where \
         each odd item is a type annotated by the following string."
        name
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_missing_funarg_annotation v ->
    let message =
      Format.sprintf "Missing a type annotation for argument \"%s\"." v.value
    in
    let location = Location.lift v.region in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_funarg_tuple_type_mismatch (region, pattern, texpr) ->
    let p = Parsing.pretty_print_pattern Parsing.Pretty.default_environment pattern |> Buffer.contents in
    let t = Parsing.pretty_print_type_expr Parsing.Pretty.default_environment texpr |> Buffer.contents in
    let message =
      Format.sprintf "The tuple \"%s\" does not have the expected type \"%s\"." p t
    in
    let location = Location.lift region in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Concrete_cameligo_type_params_not_annotated reg ->
    let message = "Functions with type parameters need to be annotated." in
    let location = Location.lift reg in
    let content = make_content ~message ~location () in
    make ~stage ~content
