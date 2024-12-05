open Ast_core

type error =
  { err_tag : error_tag
  ; err_loc : Location.t
  }

and error_tag =
  | E_unexpected_typed_tree
  | E_let_and_not_supported
  | E_type_and_not_supported
  | E_labelled_parameters_not_supported
  | E_optional_parameters_not_supported
  | E_poly_vars_not_supported
  | E_fcm_not_supported
  | E_objects_not_supported
  | E_partial_match_not_supported
  | E_exceptions_not_supported
  | E_extensible_variants_not_supported
  | E_mutation_not_supported
  | E_array_not_supported
  | E_while_not_supported
  | E_for_not_supported
  | E_refutation_not_supported
  | E_rec_modules_not_supported
  | E_lazy_not_supported
  | E_abstract_types_not_supported
  | E_abstract_module_types_not_supported
  | E_modules_without_names_not_supported
  | E_recursive_bindings_must_be_a_function
  | E_only_variable_patterns_supported
  | E_unimplemented
  | E_unsupported
  | E_unreachable
  | E_unexpected_error of exn

(* TODO: proper name for this function *)
let pp_hum_error_tag fmt error =
  let open Format in
  match error with
  | E_unexpected_typed_tree -> fprintf fmt "unexpected typed tree"
  | E_let_and_not_supported -> fprintf fmt "let and is not supported"
  | E_type_and_not_supported -> fprintf fmt "type and is not supported"
  | E_labelled_parameters_not_supported ->
    fprintf fmt "labelled parameters are not supported"
  | E_optional_parameters_not_supported ->
    fprintf fmt "optional parameters are not supported"
  | E_poly_vars_not_supported -> fprintf fmt "polymorphic variants are not supported"
  | E_fcm_not_supported -> fprintf fmt "first-class modules are not supported"
  | E_objects_not_supported -> fprintf fmt "classes and objects are not supported"
  | E_partial_match_not_supported ->
    fprintf fmt "partial pattern matching is not supported"
  | E_exceptions_not_supported -> fprintf fmt "exceptions are not supported"
  | E_extensible_variants_not_supported ->
    fprintf fmt "extensible variants are not supported"
  | E_mutation_not_supported -> fprintf fmt "mutation is not supported"
  | E_array_not_supported -> fprintf fmt "array's are not supported"
  | E_while_not_supported -> fprintf fmt "while loops are not supported"
  | E_for_not_supported -> fprintf fmt "for loops are not supported"
  | E_refutation_not_supported -> fprintf fmt "refutation's are not supported"
  | E_rec_modules_not_supported -> fprintf fmt "recursive modules are not supported"
  | E_lazy_not_supported -> fprintf fmt "lazy values are not supported"
  | E_abstract_types_not_supported -> fprintf fmt "abstract types are not supported YET"
  | E_abstract_module_types_not_supported ->
    fprintf fmt "abstract module types are not supported"
  | E_modules_without_names_not_supported ->
    fprintf fmt "modules without names not supported"
  | E_recursive_bindings_must_be_a_function ->
    fprintf fmt "recursive bindings must be a function"
  | E_only_variable_patterns_supported ->
    fprintf fmt "only variable patterns supported here"
  | E_unimplemented -> fprintf fmt "unimplemented"
  | E_unsupported -> fprintf fmt "unsupported"
  | E_unreachable -> fprintf fmt "unreachable"
  | E_unexpected_error exn -> fprintf fmt "unexpected error: %a" Core.Exn.pp exn


exception Caml_pre_error of error_tag
exception Caml_error of error

let try_enhance ~loc f =
  try f () with
  (* TODO: reraise? *)
  | Caml_pre_error tag -> raise @@ Caml_error { err_tag = tag; err_loc = loc }
  | Caml_error error as exn -> raise exn
  | exn ->
    let tag = E_unexpected_error exn in
    raise @@ Caml_error { err_tag = tag; err_loc = loc }


let try_recover ~loc ~on_error f =
  try f () with
  | Caml_pre_error tag -> on_error @@ { err_tag = tag; err_loc = loc }
  | Caml_error error -> on_error error
  | exn ->
    let tag = E_unexpected_error exn in
    on_error @@ { err_tag = tag; err_loc = loc }


let wrap_exn ~loc f =
  try Ok (f ()) with
  | Caml_pre_error tag -> Error { err_tag = tag; err_loc = loc }
  | Caml_error error -> Error error
  | exn ->
    let tag = E_unexpected_error exn in
    Error { err_tag = tag; err_loc = loc }


(* TODO: raise shadowing is a bad idea? *)
let raise_pre_error tag = raise @@ Caml_pre_error tag
let raise_error error = raise @@ Caml_error error
