open Simple_utils.Display
open Ast_imperative
open Trace

let stage = "self_ast_imperative"

type self_ast_imperative_error = [
  | `Self_ast_imperative_long_constructor of (string * type_expression)
  | `Self_ast_imperative_bad_timestamp of (string * expression)
  | `Self_ast_imperative_bad_format_literal of (expression * Proto_alpha_utils.Trace.tezos_alpha_error list)
  | `Self_ast_imperative_bad_empty_arity of (constant' * expression)
  | `Self_ast_imperative_bad_single_arity of (constant' * expression)
  | `Self_ast_imperative_bad_map_param_type of (constant' * expression)
  | `Self_ast_imperative_bad_set_param_type of (constant' * expression)
  | `Self_ast_imperative_bad_convertion_bytes of expression
]

let too_long_constructor c e = `Self_ast_imperative_long_constructor (c,e)
let bad_timestamp t e = `Self_ast_imperative_bad_timestamp (t,e)
let bad_format e errs = `Self_ast_imperative_bad_format_literal (e,errs)
let bad_empty_arity c e = `Self_ast_imperative_bad_empty_arity (c,e)
let bad_single_arity c e = `Self_ast_imperative_bad_single_arity (c,e)
let bad_map_param_type c e = `Self_ast_imperative_bad_map_param_type (c,e)
let bad_set_param_type c e = `Self_ast_imperative_bad_set_param_type (c,e)
let bad_conversion_bytes e = `Self_ast_imperative_bad_convertion_bytes e

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_ast_imperative_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_ast_imperative_long_constructor (c,e) ->
      Format.fprintf f
        "@[<hv>%a@ Too long constructor '%s'@ names length are limited to 32 (tezos limitation)@]"
        Location.pp e.location
        c
    | `Self_ast_imperative_bad_timestamp (t,e) ->
      Format.fprintf f
        "@[<hv>%a@ Badly formatted timestamp '%s'@]"
        Location.pp e.location
        t
    | `Self_ast_imperative_bad_format_literal (e,_errs) ->
      Format.fprintf f
        "@[<hv>%a@ Badly formatted literal: %a@]"
        Location.pp e.location
        Ast_imperative.PP.expression e
    | `Self_ast_imperative_bad_empty_arity (c, e) ->
      Format.fprintf f
        "@[<hv>%a@ Wrong arity:@%a expects no parameter@]"
        Location.pp e.location PP.constant c
    | `Self_ast_imperative_bad_single_arity (c, e) ->
      Format.fprintf f
        "@[<hv>%a@ Wrong arity:@%a expects one parameter@]"
        Location.pp e.location PP.constant c
    | `Self_ast_imperative_bad_map_param_type (c,e) ->
      Format.fprintf f
        "@[<hv>%a@ Wrong arity:@%a expects a list of pair parameter@]"
        Location.pp e.location PP.constant c
    | `Self_ast_imperative_bad_set_param_type (c,e) ->
      Format.fprintf f
        "@[<hv>%a@ Wrong arity:@%a expects a list of pair parameter@]"
        Location.pp e.location PP.constant c
    | `Self_ast_imperative_bad_convertion_bytes e ->
      Format.fprintf f
        "@[<hv>%a@ Bad bytes literal (conversion went wrong)@]"
        Location.pp e.location
  )

let error_jsonformat : self_ast_imperative_error -> J.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Self_ast_imperative_long_constructor (c,e) ->
    let message = `String "too long constructor (limited to 32)" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", `String c)
    ] in
    json_error ~stage ~content
  | `Self_ast_imperative_bad_timestamp (t,e) ->
    let message = `String "badly formatted timestamp" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", `String t)
    ] in
    json_error ~stage ~content
  | `Self_ast_imperative_bad_format_literal (e,_errs) ->
    let message = `String "badly formatted literal" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Self_ast_imperative_bad_empty_arity (c, e) ->
    let message = `String "constant expects no parameters" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" PP.constant c) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Self_ast_imperative_bad_single_arity (c, e) ->
    let message = `String "constant expects one parameters" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" PP.constant c) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Self_ast_imperative_bad_map_param_type (c,e) ->
    let message = `String "constant expects a list of pair as parameter" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" PP.constant c) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Self_ast_imperative_bad_set_param_type (c,e) ->
    let message = `String "constant expects a list as parameter" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" PP.constant c) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Self_ast_imperative_bad_convertion_bytes e ->
    let message = `String "Bad bytes literal (conversion went wrong)" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content