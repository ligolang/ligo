open Simple_utils.Display
module Location = Simple_utils.Location
module Snippet = Simple_utils.Snippet
open Ast_unified
open S_exp

let stage = "small_passes"

type t =
  [ `Small_passes_wrong_reduction of string
  | `Small_passes_expected_variable of ty_expr
  | `Small_passes_array_rest_not_supported of expr
  | `Small_passes_invalid_case of expr
  | `Small_passes_unsupported_match_object_property of expr
  | `Small_passes_invalid_list_pattern_match of Location.t
  | `Small_passes_michelson_type_wrong_arity of string * ty_expr
  | `Small_passes_michelson_type_wrong of string * ty_expr
  | `Small_passes_wrong_lvalue of expr
  | `Small_passes_unsupported_return of statement list
  | `Small_passes_unsupported_break of Location.t
  | `Small_passes_unsupported_continue of Location.t
  | `Small_passes_unsupported_control_flow of block
  | `Small_passes_unsupported_top_level_statement of instruction
  | `Small_passes_unsupported_import of declaration
  | `Small_passes_unsupported_object_field of expr
  | `Small_passes_unsupported_update of expr
  | `Small_passes_unsupported_rest_property of expr
  | `Small_passes_unsupported_projection of expr
  | `Small_passes_unsupported_disc_union_type of ty_expr
  | `Small_passes_recursive_no_annot of expr
  | `Small_passes_non_linear_pattern of (pattern, ty_expr) pattern_
  | `Small_passes_non_linear_type of [ `Decl of declaration | `Ty of ty_expr ty_expr_ ]
  | `Small_passes_unsupported_pattern_type of (pattern, ty_expr) pattern_
  | `Small_passes_unsupported_module_access of [ `Type of ty_expr | `Expr of expr ]
  | `Small_passes_bad_timestamp of string * expr
  | `Small_passes_bad_conversion_bytes of expr
  | `Small_passes_bad_format_literal of expr * string
  | `Small_passes_duplicate_identifier of Variable.t
  | `Small_passes_duplicate_ty_identifier of Ty_variable.t
  | `Small_passes_only_variable_in_prefix of expr
  | `Small_passes_only_variable_in_postfix of expr
  | `Small_passes_sys_error of Location.t * string
  | `Small_passes_invariant_trivial of Location.t * string
  ]
[@@deriving poly_constructor { prefix = "small_passes_" }, sexp]

let error_ppformat
    :  display_format:string display_format -> no_colour:bool -> Format.formatter -> t
    -> unit
  =
 fun ~display_format ~no_colour f a ->
  let snippet_pp = Snippet.pp ~no_colour in
  match display_format with
  (* For unit tests of small passes, we print the sexp of the full value
     because we don't have location. *)
  | Dev -> Format.fprintf f "%a" (Sexp.pp_hum_indent 4) (sexp_of_t a)
  | Human_readable ->
    (match a with
    | `Small_passes_invariant_trivial (loc, str) ->
      Format.fprintf
        f
        "@[<hv>%a@.Found an unexpected structure that could not have been reduced.@.This \
         node should have been reduced:@.%s@]"
        snippet_pp
        loc
        str
    | `Small_passes_unsupported_disc_union_type ty ->
      Format.fprintf
        f
        "@[<hv>%aAll the objects are expected to have a shared field with an unique \
         value.@]"
        snippet_pp
        (get_t_loc ty)
    | `Small_passes_wrong_reduction pass ->
      Format.fprintf f "@[<hv>Pass %s did not reduce.@]" pass
    | `Small_passes_expected_variable t ->
      Format.fprintf f "@[<hv>%a@.Expected a declaration name@]" snippet_pp (get_t_loc t)
    | `Small_passes_array_rest_not_supported e ->
      Format.fprintf
        f
        "@[<hv>%a@.Rest property not supported here.@]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_invalid_case e ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid field value. An anonymous arrow function was expected, eg. \
         Nothing: () => foo`.@]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_unsupported_match_object_property e ->
      Format.fprintf
        f
        "@[<hv>%a@.Unsupported pattern match object property.@]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_invalid_list_pattern_match loc ->
      Format.fprintf f "@[<hv>%a@.Invalid list pattern matching.@]" snippet_pp loc
    | `Small_passes_michelson_type_wrong_arity (name, t) ->
      Format.fprintf
        f
        "[@<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is \
         expected, where each odd item is a type annotated by the following string.@]"
        snippet_pp
        (get_t_loc t)
        name
    | `Small_passes_michelson_type_wrong (name, t) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a \
         string, is expected for the preceding type.@]"
        snippet_pp
        (get_t_loc t)
        name
    | `Small_passes_wrong_lvalue e ->
      Format.fprintf f "@[<hv>%a@.Unsupported lvalue@]" snippet_pp (get_e_loc e)
    | `Small_passes_unsupported_return stmts ->
      let loc =
        stmts
        |> List.map ~f:get_s_loc
        |> List.fold ~init:Location.generated ~f:Location.cover
      in
      Format.fprintf
        f
        "@[<hv>%a@.Return statement is currently not supported in this position@]"
        snippet_pp
        loc
    | `Small_passes_unsupported_break loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Break statement is not supported in that position@]"
        snippet_pp
        loc
    | `Small_passes_unsupported_continue loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Continue statement is currently not supported@]"
        snippet_pp
        loc
    | `Small_passes_unsupported_control_flow block ->
      let loc = get_b_loc block in
      Format.fprintf
        f
        "@[<hv>%a@.Control flow statements are not supported in sub-blocks@]"
        snippet_pp
        loc
    | `Small_passes_unsupported_top_level_statement i ->
      let loc = get_i_loc i in
      Format.fprintf f "@[<hv>%a@.Unsupported top-level statement@]" snippet_pp loc
    | `Small_passes_unsupported_import d ->
      let loc = get_d_loc d in
      Format.fprintf f "@[<hv>%a@.Unsupported import directive@]" snippet_pp loc
    | `Small_passes_unsupported_object_field e ->
      Format.fprintf f "@[<hv>%a@.Unsupported object field@]" snippet_pp (get_e_loc e)
    | `Small_passes_unsupported_update e ->
      Format.fprintf f "@[<hv>%a@.Unsupported update@]" snippet_pp (get_e_loc e)
    | `Small_passes_unsupported_rest_property e ->
      Format.fprintf f "@[<hv>%a@.Unsupported rest property@]" snippet_pp (get_e_loc e)
    | `Small_passes_recursive_no_annot e ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid function declaration. Recursive functions are required to \
         have a type annotation@]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_non_linear_pattern p ->
      Format.fprintf
        f
        "@[<v>%a@.Repeated variable in pattern.@.Hint: Change the name.@]"
        snippet_pp
        (Location.get_location p)
    | `Small_passes_non_linear_type t ->
      Format.fprintf
        f
        "@[<v>%a@.Repeated type variable in type.@.Hint: Change the name.@]"
        snippet_pp
        (match t with
        | `Decl d -> get_d_loc d
        | `Ty t -> Location.get_location t)
    | `Small_passes_unsupported_pattern_type p ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid pattern matching.@.  If this is pattern matching over \
         Booleans, then \"true\" or \"false\" is expected.@.  If this is pattern \
         matching on a list, then one of the following is expected:@.    * an empty list \
         pattern \"[]\";@.    * a cons list pattern \"[head, ...tail]\".@.  If this is \
         pattern matching over variants, then a constructor of a variant is \
         expected.@.@.  Other forms of pattern matching are not (yet) supported. @]"
        snippet_pp
        (Location.get_location p)
    | `Small_passes_unsupported_module_access access ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid access. A variable is expected @]"
        snippet_pp
        ((function
           | `Expr e -> get_e_loc e
           | `Type t -> get_t_loc t)
           access)
    | `Small_passes_bad_timestamp (t, e) ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed timestamp \"%s\".@.At this point, a string with a RFC3339 \
         notation or the number of seconds since Epoch is expected. @]"
        snippet_pp
        (get_e_loc e)
        t
    | `Small_passes_bad_conversion_bytes e ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed bytes literal.@.Example of a valid bytes literal: \
         \"ff7a7aff\". @]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_unsupported_projection e ->
      Format.fprintf
        f
        "@[<hv>%a@ Unsupported projection, expected an int@]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_bad_format_literal (e, s) ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed literal \"%s\".@.In the case of an address, a string is \
         expected prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 \
         encoded hash and terminated by a 4-byte checksum.@.In the case of a key_hash, \
         signature, or key a Base58 encoded hash is expected. @]"
        snippet_pp
        (get_e_loc e)
        s
    | `Small_passes_duplicate_identifier x ->
      let loc = Variable.get_location x in
      Format.fprintf f "@[<hv>%a@ Duplicate identifier. @]" snippet_pp loc
    | `Small_passes_duplicate_ty_identifier x ->
      let loc = Ty_variable.get_location x in
      Format.fprintf f "@[<hv>%a@ Duplicate identifier. @]" snippet_pp loc
    | `Small_passes_only_variable_in_prefix e ->
      Format.fprintf
        f
        "@[<hv>%a@ Only variables are accepted in prefix operators. @]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_only_variable_in_postfix e ->
      Format.fprintf
        f
        "@[<hv>%a@ Only variables are accepted in postfix operators. @]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_sys_error (l, msg) ->
      Format.fprintf f "@[<hv>%a@ Found a system error: %s. @]" snippet_pp l msg)


let error_json : t -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Small_passes_invariant_trivial (location, str) ->
    let message =
      Format.asprintf
        "Found an unexpected structure that could not have been reduced.@.This node \
         should have been reduced:@.%s"
        str
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_wrong_reduction pass ->
    let message = Format.asprintf "@[<hv>Pass %s did not reduce.@]" pass in
    let content = make_content ~message () in
    make ~stage ~content
  | `Small_passes_expected_variable t ->
    let message = Format.asprintf "Expected a declaration name." in
    let content = make_content ~message ~location:(get_t_loc t) () in
    make ~stage ~content
  | `Small_passes_array_rest_not_supported e ->
    let message = Format.asprintf "Rest property not supported here." in
    let content = make_content ~message ~location:(get_e_loc e) () in
    make ~stage ~content
  | `Small_passes_invalid_case e ->
    let message =
      "Invalid field value. An anonymous arrow function was expected, eg. Nothing: () => \
       foo`."
    in
    let location = get_e_loc e in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_unsupported_match_object_property e ->
    let message = "Unsupported pattern match object property" in
    let location = get_e_loc e in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_invalid_list_pattern_match loc ->
    let message = "Invalid list pattern matching" in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Small_passes_michelson_type_wrong_arity (name, t) ->
    let message =
      Format.sprintf
        "Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where \
         each odd item is a type annotated by the following string."
        name
    in
    let location = get_t_loc t in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_michelson_type_wrong (name, t) ->
    let message =
      Format.sprintf
        "Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is \
         expected for the preceding type."
        name
    in
    let location = get_t_loc t in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_wrong_lvalue e ->
    let location = get_e_loc e in
    let content =
      make_content ~message:"Expected a field name or an accessor" ~location ()
    in
    make ~stage ~content
  | `Small_passes_unsupported_return stmts ->
    let location =
      stmts
      |> List.map ~f:get_s_loc
      |> List.fold ~init:Location.generated ~f:Location.cover
    in
    let content =
      make_content
        ~message:"Return statement is currently not supported in this position"
        ~location
        ()
    in
    make ~stage ~content
  | `Small_passes_unsupported_break location ->
    let content =
      make_content
        ~message:"Break statement is not supported in that position"
        ~location
        ()
    in
    make ~stage ~content
  | `Small_passes_unsupported_continue location ->
    let content =
      make_content ~message:"Continue statement is currently not supported" ~location ()
    in
    make ~stage ~content
  | `Small_passes_unsupported_control_flow block ->
    let location = get_b_loc block in
    let content =
      make_content ~message:"Control flow is not supported within sub-blocks" ~location ()
    in
    make ~stage ~content
  | `Small_passes_unsupported_top_level_statement i ->
    let location = get_i_loc i in
    let content = make_content ~message:"Unsupported top-level statement" ~location () in
    make ~stage ~content
  | `Small_passes_unsupported_import d ->
    let location = get_d_loc d in
    let content = make_content ~message:"Unsupported import directive" ~location () in
    make ~stage ~content
  | `Small_passes_unsupported_object_field e ->
    let location = get_e_loc e in
    let content = make_content ~message:"Unsupported object field" ~location () in
    make ~stage ~content
  | `Small_passes_unsupported_update e ->
    let location = get_e_loc e in
    let content = make_content ~message:"Unsupported update" ~location () in
    make ~stage ~content
  | `Small_passes_unsupported_rest_property e ->
    let location = get_e_loc e in
    let content = make_content ~message:"Unsupported rest property" ~location () in
    make ~stage ~content
  | `Small_passes_recursive_no_annot e ->
    let location = get_e_loc e in
    let content =
      make_content
        ~message:"Recursive functions are required to have a type annotation"
        ~location
        ()
    in
    make ~stage ~content
  | `Small_passes_non_linear_pattern p ->
    let message =
      Format.sprintf "Repeated variable in pattern.@.Hint: Change the name."
    in
    let content = make_content ~message ~location:(Location.get_location p) () in
    make ~stage ~content
  | `Small_passes_non_linear_type t ->
    let message =
      Format.sprintf "Repeated type variable in type.@.Hint: Change the name."
    in
    let content =
      make_content
        ~message
        ~location:
          (match t with
          | `Decl d -> get_d_loc d
          | `Ty t -> Location.get_location t)
        ()
    in
    make ~stage ~content
  | `Small_passes_unsupported_pattern_type p ->
    let message = Format.sprintf "Unsupported pattern type" in
    let content = make_content ~message ~location:(Location.get_location p) () in
    make ~stage ~content
  | `Small_passes_unsupported_module_access access ->
    let message = "Invalid access. A variable is expected" in
    let location =
      (function
        | `Expr e -> get_e_loc e
        | `Type t -> get_t_loc t)
        access
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_bad_timestamp (t, e) ->
    let message =
      Format.sprintf
        "Ill-formed timestamp \"%s\".@.At this point, a string with a RFC3339 notation \
         or the number of seconds since Epoch is expected."
        t
    in
    let content = make_content ~message ~location:(get_e_loc e) () in
    make ~stage ~content
  | `Small_passes_bad_conversion_bytes e ->
    let message =
      Format.sprintf
        "Ill-formed bytes literal.@.Example of a valid bytes literal: \"ff7a7aff\"."
    in
    let content = make_content ~message ~location:(get_e_loc e) () in
    make ~stage ~content
  | `Small_passes_unsupported_projection e ->
    let message = "Unsupported projection" in
    let content = make_content ~message ~location:(get_e_loc e) () in
    make ~stage ~content
  | `Small_passes_bad_format_literal (e, s) ->
    let message =
      Format.asprintf
        "Ill-formed literal \"%s\".@.In the case of an address, a string is expected \
         prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash \
         and terminated by a 4-byte checksum.@.In the case of a key_hash, signature, or \
         key a Base58 encoded hash is expected."
        s
    in
    let location = get_e_loc e in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_duplicate_identifier x ->
    let location = Variable.get_location x in
    let message = "Duplicate Identifier" in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_duplicate_ty_identifier x ->
    let location = Ty_variable.get_location x in
    let message = "Duplicate Identifier" in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_unsupported_disc_union_type ty ->
    let location = get_t_loc ty in
    let message = "Unsupported disc union type" in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_only_variable_in_prefix e ->
    let location = get_e_loc e in
    let message = "Only variables are accepted in prefix operators" in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_only_variable_in_postfix e ->
    let location = get_e_loc e in
    let message = "Only variables are accepted in postfix operators" in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_sys_error (location, msg) ->
    let message = Format.sprintf "Found a system error: %s" msg in
    let content = make_content ~message ~location () in
    make ~stage ~content
