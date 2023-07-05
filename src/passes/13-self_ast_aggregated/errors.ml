open Simple_utils.Display
open Ligo_prim

let stage = "self_ast_aggregated"

type self_ast_aggregated_error =
  [ `Self_ast_aggregated_expected_obj_ligo of Location.t
  | `Self_ast_aggregated_expected_obj_ligo_type of
    Location.t * Ast_aggregated.type_expression * Ast_aggregated.type_expression
  | `Self_ast_aggregated_polymorphism_unresolved of Location.t
  | `Self_ast_aggregated_monomorphisation_non_var of Ast_aggregated.expression
  | `Self_ast_aggregated_monomorphisation_non_for_all of Ast_aggregated.expression
  | `Self_ast_aggregated_monomorphisation_unexpected_type_abs of
    Ast_aggregated.type_expression * Ast_aggregated.expression
  | `Self_ast_aggregated_fvs_in_create_contract_lambda of
    Ast_aggregated.expression * Value_var.t
  | `Self_ast_aggregated_create_contract_lambda of
    Constant.constant' * Ast_aggregated.expression
  | `Self_ast_aggregated_bad_format_entrypoint_ann of string * Location.t
  | `Self_ast_aggregated_entrypoint_ann_not_literal of Location.t
  | `Self_ast_aggregated_emit_tag_not_literal of Location.t
  | `Self_ast_aggregated_call_view_not_litstr of Location.t
  | `Self_ast_aggregated_unmatched_entrypoint of Location.t
  | `Self_ast_aggregated_corner_case of string
  | `Self_ast_aggregated_bad_single_arity of
    Constant.constant' * Ast_aggregated.expression
  | `Self_ast_aggregated_bad_map_param_type of
    Constant.constant' * Ast_aggregated.expression
  | `Self_ast_aggregated_bad_set_param_type of
    Constant.constant' * Ast_aggregated.expression
  | `Self_ast_aggregated_nested_bigmap of Location.t
  | `Self_ast_aggregated_unsolved_coerce of Location.t
  ]
[@@deriving poly_constructor { prefix = "self_ast_aggregated_" }]

let error_ppformat
    :  display_format:string display_format -> no_colour:bool -> Format.formatter
    -> self_ast_aggregated_error -> unit
  =
 fun ~display_format ~no_colour f a ->
  let name_tbl = Ast_aggregated.PP.With_name_tbl.Type_var_name_tbl.create () in
  let pp_type = Ast_aggregated.PP.With_name_tbl.pp_with_name_tbl ~tbl:name_tbl in
  let snippet_pp = Snippet.pp ~no_colour in
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Self_ast_aggregated_expected_obj_ligo loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid usage of a Test primitive: cannot be translated to \
         Michelson.@]"
        snippet_pp
        loc
    | `Self_ast_aggregated_expected_obj_ligo_type (loc, local, global) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid usage of a Test type: %a in %a cannot be translated to \
         Michelson.@]"
        snippet_pp
        loc
        Ast_aggregated.PP.type_expression
        local
        Ast_aggregated.PP.type_expression
        global
    | `Self_ast_aggregated_polymorphism_unresolved loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Can't infer the type of this value, please add a type annotation.@]"
        snippet_pp
        loc
    | `Self_ast_aggregated_monomorphisation_non_var expr
    | `Self_ast_aggregated_monomorphisation_non_for_all expr ->
      if Location.is_dummy_or_generated expr.location
      then
        Format.fprintf
          f
          "@[<hv>%a@.Cannot monomorphise the expression.@]"
          Ast_aggregated.PP.expression
          expr
      else
        Format.fprintf
          f
          "@[<hv>%a@.Cannot monomorphise the expression.@]"
          snippet_pp
          expr.location
    | `Self_ast_aggregated_monomorphisation_unexpected_type_abs (ty, expr) ->
      if Location.is_dummy_or_generated expr.location
      then
        Format.fprintf
          f
          "@[<hv>%a@.Cannot monomorphise the expression.@.The inferred type was \
           \"%a\".@.Hint: Try adding additional annotations.@]"
          Ast_aggregated.PP.expression
          expr
          pp_type
          ty
      else
        Format.fprintf
          f
          "@[<hv>%a@.Cannot monomorphise the expression.@.The inferred type was \
           \"%a\".@.Hint: Try adding additional annotations.@]"
          snippet_pp
          expr.location
          pp_type
          ty
    | `Self_ast_aggregated_fvs_in_create_contract_lambda (e, v) ->
      Format.fprintf
        f
        "@[<hv>%a@.Free variable usage is not allowed in call to \
         Tezos.create_contract:@.%a@]"
        snippet_pp
        e.location
        snippet_pp
        (Value_var.get_location v)
    | `Self_ast_aggregated_create_contract_lambda (_cst, e) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid usage of Tezos.create_contract.@.The first argument must be \
         an inline function. @]"
        snippet_pp
        e.location
    | `Self_ast_aggregated_bad_format_entrypoint_ann (ep, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid entrypoint \"%s\". One of the following patterns is \
         expected:@.* \"%%bar\" is expected for entrypoint \"Bar\"@.* \"%%default\" when \
         no entrypoint is used.@.Valid characters in annotation: ('a' .. 'z' | 'A' .. \
         'Z' | '_' | '.' | '%%' | '@' | '0' .. '9')."
        snippet_pp
        loc
        ep
    | `Self_ast_aggregated_entrypoint_ann_not_literal loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value must be a string \
         literal. @]"
        snippet_pp
        loc
    | `Self_ast_aggregated_call_view_not_litstr loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid argument.@.View name must be a string literal. @]"
        snippet_pp
        loc
    | `Self_ast_aggregated_emit_tag_not_literal loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid event tag.@.The tag must be a string literal. @]"
        snippet_pp
        loc
    | `Self_ast_aggregated_unmatched_entrypoint loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value does not match a \
         constructor of the contract parameter. @]"
        snippet_pp
        loc
    | `Self_ast_aggregated_corner_case desc ->
      Format.fprintf f "@[<hv>Internal error: %s @]" desc
    | `Self_ast_aggregated_bad_single_arity (c, e) ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed \"%a\" expression@.One function argument is expected. @]"
        snippet_pp
        e.location
        Constant.pp_constant'
        c
    | `Self_ast_aggregated_bad_map_param_type (c, e) ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed \"%a\" expression.@.A list of pair parameters is \
         expected.@]"
        snippet_pp
        e.location
        Constant.pp_constant'
        c
    | `Self_ast_aggregated_bad_set_param_type (c, e) ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed \"%a\" expression.@.A list of pair parameters is \
         expected.@]"
        snippet_pp
        e.location
        Constant.pp_constant'
        c
    | `Self_ast_aggregated_nested_bigmap loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid big map nesting.@.A big map cannot be nested inside another \
         big map. @]"
        snippet_pp
        loc
    | `Self_ast_aggregated_unsolved_coerce loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid coercion. It should have been resolved.@]"
        snippet_pp
        loc)

let error_json : self_ast_aggregated_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Self_ast_aggregated_expected_obj_ligo location ->
    let message = "Invalid usage of a Test primitive." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_expected_obj_ligo_type (location, local, global) ->
    let message =
      Format.asprintf
        "Invalid usage of a Test type: %a in %a cannot be translated to Michelson."
        Ast_aggregated.PP.type_expression
        local
        Ast_aggregated.PP.type_expression
        global
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_polymorphism_unresolved location ->
    let message = "Can't infer the type of this value, please add a type annotation." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_monomorphisation_non_var expr
  | `Self_ast_aggregated_monomorphisation_unexpected_type_abs (_, expr)
  | `Self_ast_aggregated_monomorphisation_non_for_all expr ->
    let message = "Cannot monomorphise the expression." in
    let content = make_content ~message ~location:expr.location () in
    make ~stage ~content
  | `Self_ast_aggregated_fvs_in_create_contract_lambda (_, v) ->
    let location = Value_var.get_location v in
    let message =
      "Free variable usage is not allowed in call to Tezos.create_contract."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_create_contract_lambda (_cst, e) ->
    let location = e.location in
    let message =
      Format.sprintf
        "Invalid usage of Tezos.create_contract.@.The first argument must be an inline \
         function."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_bad_format_entrypoint_ann (ep, location) ->
    let message =
      Format.sprintf
        "Invalid entrypoint \"%s\". One of the following patterns is expected:@.* \
         \"%%bar\" is expected for entrypoint \"Bar\"@.* \"%%default\" when no \
         entrypoint is used."
        ep
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_entrypoint_ann_not_literal location ->
    let message =
      Format.sprintf
        "Invalid entrypoint value.@.The entrypoint value must be a string literal."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_call_view_not_litstr location ->
    let message =
      Format.sprintf "Invalid argument.@.View name must be a string literal."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_emit_tag_not_literal location ->
    let message =
      Format.sprintf "Invalid event tag.@.The tag must be a string literal."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_unmatched_entrypoint location ->
    let message =
      Format.sprintf
        "Invalid entrypoint value.@.The entrypoint value does not match a constructor of \
         the contract parameter."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_corner_case desc ->
    let message = Format.sprintf "Internal error: %s" desc in
    let content = make_content ~message () in
    make ~stage ~content
  | `Self_ast_aggregated_bad_single_arity (c, e) ->
    let location = e.location in
    let message =
      Format.asprintf
        "Ill-formed \"%a\" expression@.One function argument is expected."
        Constant.pp_constant'
        c
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_bad_map_param_type (c, e) ->
    let location = e.location in
    let message =
      Format.asprintf
        "Ill-formed \"%a\" expression.@.A list of pair parameters is expected."
        Constant.pp_constant'
        c
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_bad_set_param_type (c, e) ->
    let location = e.location in
    let message =
      Format.asprintf
        "Ill-formed \"%a\" expression.@.A list of pair parameters is expected."
        Constant.pp_constant'
        c
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_nested_bigmap location ->
    let message =
      Format.sprintf
        "Invalid big map nesting.@.A big map cannot be nested inside another big map."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_unsolved_coerce location ->
    let message =
      Format.sprintf
        "Unsolved coercion."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
