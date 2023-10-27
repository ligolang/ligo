module Snippet = Simple_utils.Snippet
module Location = Simple_utils.Location
module PP = Ast_typed.PP
open Simple_utils.Display
open Ligo_prim

let stage = "self_ast_typed"

type self_ast_typed_error =
  [ `Self_ast_typed_corner_case of string
  | `Self_ast_typed_illegal_non_initial_dynamic of Location.t
  | `Self_ast_typed_not_a_contract of string
  | `Self_ast_typed_bad_view_io of Module_var.t * Location.t
  | `Self_ast_typed_bad_view_storage of
    Module_var.t * Ast_typed.type_expression * Location.t
  | `Self_ast_typed_bad_view_not_a_function of string * Location.t
  | `Self_ast_typed_bad_view_too_few_arguments of string * Location.t
  | `Self_ast_typed_storage_view_contract of
    Location.t
    * Module_var.t
    * Value_var.t
    * Ast_typed.type_expression
    * Ast_typed.type_expression
  | `Self_ast_typed_view_io of Location.t * Ast_typed.type_expression * [ `In | `Out ]
  ]
[@@deriving poly_constructor { prefix = "self_ast_typed_" }]

let type_view_io_in loc got = view_io loc got `In
let type_view_io_out loc got = view_io loc got `Out

let error_ppformat
    :  display_format:string display_format -> no_colour:bool -> Format.formatter
    -> self_ast_typed_error -> unit
  =
 fun ~display_format ~no_colour f a ->
  let snippet_pp = Snippet.pp ~no_colour in
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Self_ast_typed_not_a_contract str ->
      (match str with
      | "" -> Format.fprintf f "File is not a contract"
      | _ -> Format.fprintf f "%s is not a contract" str)
    | `Self_ast_typed_storage_view_contract (loc, main_name, view_name, ct, vt) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid view argument.@.View '%a' has storage type '%a' and contract \
         '%a' has storage type '%a'.@]"
        snippet_pp
        loc
        Value_var.pp
        view_name
        Ast_typed.PP.type_expression
        vt
        Module_var.pp
        main_name
        Ast_typed.PP.type_expression
        ct
    | `Self_ast_typed_illegal_non_initial_dynamic loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Illegal position for opted out entry.@. Only allowed in contracts \
         \"@dyn_entry\" top-level declarations right-end side.@]"
        snippet_pp
        loc
    | `Self_ast_typed_view_io (loc, got, arg) ->
      let s =
        match arg with
        | `In -> "input"
        | `Out -> "output"
      in
      Format.fprintf
        f
        "@[<hv>%a@.Invalid view.@.Type '%a' is forbidden as %s argument.@]"
        snippet_pp
        loc
        Ast_typed.PP.type_expression
        got
        s
    | `Self_ast_typed_corner_case desc ->
      Format.fprintf f "@[<hv>Internal error: %s @]" desc
    | `Self_ast_typed_bad_view_io (entrypoint, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type for view \"%a\".@.A view must be a function. @]"
        snippet_pp
        loc
        Module_var.pp
        entrypoint
    | `Self_ast_typed_bad_view_storage (entrypoint, storage_ty, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type for view \"%a\".@.Cannot find \"%a\" as storage. @]"
        snippet_pp
        loc
        Module_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        storage_ty
    | `Self_ast_typed_bad_view_not_a_function (name, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.The view \"%s\" is not a function.@.Views must be functions taking \
         exactly two arguments: their input, and the storage. @]"
        snippet_pp
        loc
        name
    | `Self_ast_typed_bad_view_too_few_arguments (name, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.The view \"%s\" has too few parameters.@.Views must be functions \
         taking exactly two arguments: their input, and the storage.@.If you get this \
         error while migrating a contract to a new version of LIGO, this is likely due \
         to the depreciation of uncurried views. See the documentation on migration to \
         LIGO v. 1.0. @]"
        snippet_pp
        loc
        name)


let error_json : self_ast_typed_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Self_ast_typed_not_a_contract _ ->
    let message = "No contract found" in
    let content = make_content ~message () in
    make ~stage ~content
  | `Self_ast_typed_illegal_non_initial_dynamic location ->
    let message = "Illegal non initial dynamic entrypoints" in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_storage_view_contract (location, main_name, view_name, ct, vt) ->
    let message =
      Format.asprintf
        "Invalid view argument.@.View '%a' has storage type '%a' and contract '%a' has \
         storage type '%a'."
        Value_var.pp
        view_name
        Ast_typed.PP.type_expression
        vt
        Module_var.pp
        main_name
        Ast_typed.PP.type_expression
        ct
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_view_io (location, got, arg) ->
    let s =
      match arg with
      | `In -> "input"
      | `Out -> "output"
    in
    let message =
      Format.asprintf
        "Invalid view.@.Type '%a' is forbidden as %s argument."
        Ast_typed.PP.type_expression
        got
        s
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_bad_view_io (entrypoint, location) ->
    let message =
      Format.asprintf
        "Invalid type for view \"%a\".@.A view must be a function."
        Module_var.pp
        entrypoint
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_bad_view_storage (entrypoint, storage_ty, location) ->
    let message =
      Format.asprintf
        "Invalid type for view \"%a\".@.Cannot find \"%a\" as storage."
        Module_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        storage_ty
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_bad_view_not_a_function (name, location) ->
    let message = Format.asprintf "The view \"%s\" is not a function." name in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_bad_view_too_few_arguments (name, location) ->
    let message = Format.asprintf "The view \"%s\" has too few parameters." name in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_corner_case desc ->
    let message = Format.sprintf "Internal error: %s" desc in
    let content = make_content ~message () in
    make ~stage ~content
