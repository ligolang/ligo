module Snippet = Simple_utils.Snippet
module PP_helpers = Simple_utils.PP_helpers
open Simple_utils.Display
open Ligo_prim
open Ast_imperative

let stage = "self_ast_imperative"

type self_ast_imperative_error =
  [ `Self_ast_imperative_long_constructor of string * type_expression
  | `Self_ast_imperative_bad_timestamp of string * expression
  | `Self_ast_imperative_bad_format_literal of expression
  | `Self_ast_imperative_bad_conversion_bytes of expression
  | `Self_ast_imperative_vars_captured of (Location.t * Value_var.t) list
  | `Self_ast_imperative_const_assigned of Location.t * Value_var.t
  | `Self_ast_imperative_no_shadowing of Location.t
  | `Self_ast_imperative_non_linear_pattern of type_expression option Pattern.t
  | `Self_ast_imperative_non_linear_record of expression
  | `Self_ast_imperative_non_linear_type_decl of type_expression
  | `Self_ast_imperative_non_linear_row of type_expression
  | `Self_ast_imperative_duplicate_parameter of expression
  | `Self_ast_imperative_reserved_name of string * Location.t
  ]
[@@deriving poly_constructor { prefix = "self_ast_imperative_" }]

let error_ppformat
    :  display_format:string display_format -> no_colour:bool -> Format.formatter
    -> self_ast_imperative_error -> unit
  =
 fun ~display_format ~no_colour f a ->
  let snippet_pp = Snippet.pp ~no_colour in
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Self_ast_imperative_non_linear_record e ->
      Format.fprintf
        f
        "@[<v>%a@.Duplicated record field@.Hint: Change the name.@]"
        snippet_pp
        e.location
    | `Self_ast_imperative_reserved_name (str, loc) ->
      Format.fprintf
        f
        "@[<v>%a@.Reserved name %S.@.Hint: Change the name.@]"
        snippet_pp
        loc
        str
    | `Self_ast_imperative_non_linear_pattern t ->
      Format.fprintf
        f
        "@[<v>%a@.Repeated variable in pattern.@.Hint: Change the name.@]"
        snippet_pp
        t.location
    | `Self_ast_imperative_non_linear_type_decl t ->
      Format.fprintf
        f
        "@[<v>%a@.Repeated type variable in type.@.Hint: Change the name.@]"
        snippet_pp
        t.location
    | `Self_ast_imperative_non_linear_row t ->
      Format.fprintf
        f
        "@[<v>%a@.Duplicated field or variant name.@.Hint: Change the name.@]"
        snippet_pp
        t.location
    | `Self_ast_imperative_duplicate_parameter exp ->
      Format.fprintf
        f
        "@[<v>%a@.Duplicated variable name in function parameter.@.Hint: Change the \
         name.@]"
        snippet_pp
        exp.location
    | `Self_ast_imperative_long_constructor (c, e) ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed data constructor \"%s\".@.Data constructors have a maximum \
         length of 32 characters, which is a limitation imposed by annotations in Tezos. \
         @]"
        snippet_pp
        e.location
        c
    | `Self_ast_imperative_bad_timestamp (t, e) ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed timestamp \"%s\".@.At this point, a string with a RFC3339 \
         notation or the number of seconds since Epoch is expected. @]"
        snippet_pp
        e.location
        t
    | `Self_ast_imperative_bad_format_literal e ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed literal \"%a\".@.In the case of an address, a string is \
         expected prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 \
         encoded hash and terminated by a 4-byte checksum.@.In the case of a key_hash, \
         signature, or key a Base58 encoded hash is expected. @]"
        snippet_pp
        e.location
        Ast_imperative.PP.expression
        e
    | `Self_ast_imperative_bad_conversion_bytes e ->
      Format.fprintf
        f
        "@[<hv>%a@ Ill-formed bytes literal.@.Example of a valid bytes literal: \
         \"ff7a7aff\". @]"
        snippet_pp
        e.location
    | `Self_ast_imperative_vars_captured vars ->
      let pp_var ppf ((decl_loc, var) : Location.t * Value_var.t) =
        Format.fprintf
          ppf
          "@[<hv>%a@ Invalid capture of non-constant variable \"%a\", declared at@.%a@]"
          snippet_pp
          (Value_var.get_location var)
          Value_var.pp
          var
          snippet_pp
          decl_loc
      in
      Format.fprintf f "%a" (PP_helpers.list_sep pp_var (PP_helpers.tag "@.")) vars
    | `Self_ast_imperative_const_assigned (loc, var) ->
      Format.fprintf
        f
        "@[<hv>%a@ Invalid assignment to constant variable \"%a\", declared at@.%a@]"
        snippet_pp
        loc
        Value_var.pp
        var
        snippet_pp
        (Value_var.get_location var)
    | `Self_ast_imperative_no_shadowing l ->
      Format.fprintf f "@[<hv>%a@ Cannot redeclare block-scoped variable. @]" snippet_pp l)


let error_json : self_ast_imperative_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Self_ast_imperative_non_linear_record e ->
    let message = Format.sprintf "Duplicated record field@.Hint: Change the name." in
    let location = e.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_reserved_name (str, location) ->
    let message = Format.sprintf "Reserved name %S.@.Hint: Change the name." str in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_non_linear_pattern t ->
    let message =
      Format.sprintf "Repeated variable in pattern.@.Hint: Change the name."
    in
    let location = t.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_non_linear_type_decl t ->
    let message =
      Format.sprintf "Repeated type variable in type.@.Hint: Change the name."
    in
    let location = t.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_non_linear_row t ->
    let message =
      Format.sprintf "Duplicated field or variant name.@.Hint: Change the name."
    in
    let location = t.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_duplicate_parameter exp ->
    let message =
      Format.sprintf
        "Duplicated variable name in function parameter.@.Hint: Change the name."
    in
    let location = exp.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_long_constructor (c, e) ->
    let message =
      Format.sprintf
        " Ill-formed data constructor \"%s\".@.Data constructors have a maximum length \
         of 32 characters, which is a limitation imposed by annotations in Tezos."
        c
    in
    let location = e.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_bad_timestamp (t, e) ->
    let message =
      Format.sprintf
        "Ill-formed timestamp \"%s\".@.At this point, a string with a RFC3339 notation \
         or the number of seconds since Epoch is expected."
        t
    in
    let location = e.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_bad_format_literal e ->
    let message =
      Format.asprintf
        "Ill-formed literal \"%a\".@.In the case of an address, a string is expected \
         prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash \
         and terminated by a 4-byte checksum.@.In the case of a key_hash, signature, or \
         key a Base58 encoded hash is expected."
        Ast_imperative.PP.expression
        e
    in
    let location = e.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_bad_conversion_bytes e ->
    let message =
      Format.sprintf
        "Ill-formed bytes literal.@.Example of a valid bytes literal: \"ff7a7aff\"."
    in
    let location = e.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_vars_captured vars ->
    let message = "Invalid capture of non-constant variable" in
    let location = List.hd vars |> Option.map ~f:(fun (l, _) -> l) in
    let content = make_content ~message ?location () in
    make ~stage ~content
  | `Self_ast_imperative_const_assigned (location, var) ->
    let message =
      Format.asprintf "Invalid assignment to constant variable \"%a\"" Value_var.pp var
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_imperative_no_shadowing location ->
    let message = "Cannot redeclare block-scoped variable." in
    let content = make_content ~message ~location () in
    make ~stage ~content
