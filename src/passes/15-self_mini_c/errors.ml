open Simple_utils.Display
open Ligo_prim

let stage = "self_mini_c"

type self_mini_c_error = [
  | `Self_mini_c_bad_self_address
  | `Self_mini_c_not_a_function
  | `Self_mini_c_not_a_pair
  | `Self_mini_c_could_not_aggregate_entry
  | `Self_mini_c_corner_case of string
  | `Self_mini_c_fvs_in_create_contract_lambda of Mini_c.expression * Value_var.t
  | `Self_mini_c_create_contract_lambda of Constant.constant' * Mini_c.expression
  | `Self_mini_c_not_comparable of string * Mini_c.type_expression
  | `Self_mini_c_bad_capture of Simple_utils.Location.t * Mini_c.type_expression
] [@@deriving poly_constructor { prefix = "self_mini_c_" }]

let error_ppformat : display_format:string display_format -> no_colour:bool ->
  Format.formatter -> self_mini_c_error -> unit =
  fun ~display_format ~no_colour f a ->
  let snippet_pp = Simple_utils.Snippet.pp ~no_colour in
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_mini_c_corner_case str ->
      Format.fprintf f "%s" str
    | `Self_mini_c_bad_self_address ->
      let s = Format.asprintf "\"Tezos.self\" must be used directly and cannot be used via another function." in
      Format.pp_print_string f s ;
    | `Self_mini_c_not_a_function -> Format.fprintf f "Invalid type for entrypoint.@.An entrypoint must of type \"parameter * storage -> operation list * storage\"."
    | `Self_mini_c_not_a_pair -> Format.fprintf f "Invalid type for entrypoint.@.An entrypoint must of type \"a * storage -> b\"."
    | `Self_mini_c_could_not_aggregate_entry -> Format.fprintf f "Invalid type for entrypoint.@.An entrypoint must of type \"parameter * storage -> operation list * storage\"."
    | `Self_mini_c_fvs_in_create_contract_lambda (e,v) ->
      Format.fprintf f
        "@[<hv>%a@.Not all free variables could be inlined in Tezos.create_contract usage: %a.@]"
        snippet_pp e.location
        Value_var.pp v
    | `Self_mini_c_create_contract_lambda (_cst,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of Tezos.create_contract.@.The first argument must be an inline function. @]"
        snippet_pp e.location
    | `Self_mini_c_not_comparable (s, t) ->
      Format.fprintf f
        "@[<hv>%a@.The %s constructor needs a comparable type argument, but it was given a non-comparable one.@]"
        snippet_pp t.location s
    | `Self_mini_c_bad_capture (l, t) ->
      let pp_type ppf (t : Mini_c.type_expression) =
        match t.source_type with
        | None -> Format.fprintf ppf "%a" Mini_c.PP.type_expression t
        | Some t -> Format.fprintf ppf "%a" Ast_typed.PP.type_expression t
      in
      Format.fprintf
        f
        "@[<hv>%a@.Invalid capturing, term captures the type %a.@.Hint: Uncurry or use \
         tuples instead of high-order functions.@]"
        snippet_pp
        l
        pp_type
        t
  )

  let error_json : self_mini_c_error -> Simple_utils.Error.t = fun e ->
    let open Simple_utils.Error in
    match e with
    | `Self_mini_c_corner_case message ->
      let content = make_content ~message () in
      make ~stage ~content
    | `Self_mini_c_bad_self_address ->
      let message = "\"Tezos.self\" must be used directly and cannot be used via another function." in
      let content = make_content ~message () in
      make ~stage ~content
    | `Self_mini_c_could_not_aggregate_entry ->
      let message = Format.sprintf "Invalid type for entrypoint.@.An entrypoint must of type \"parameter * storage -> operation list * storage\"." in
      let content = make_content ~message () in
      make ~stage ~content
    | `Self_mini_c_not_a_pair ->
      let message = Format.sprintf  "Invalid type for entrypoint.@.An entrypoint must of type \"a * storage -> b\"." in
      let content = make_content ~message () in
      make ~stage ~content
    | `Self_mini_c_not_a_function ->
      let message = Format.sprintf  "Invalid type for entrypoint.@.An entrypoint must of type \"parameter * storage -> operation list * storage\"." in
      let content = make_content ~message () in
      make ~stage ~content
    | `Self_mini_c_fvs_in_create_contract_lambda (e, v) ->
      let message = Format.asprintf "Not all free variables could be inlined in Tezos.create_contract usage: %a" Value_var.pp v in
      let location = e.location in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Self_mini_c_create_contract_lambda (_,e) ->
      let message = Format.sprintf  "Invalid usage of Tezos.create_contract.@.The first argument must be an inline function." in
      let location = e.location in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Self_mini_c_not_comparable (s, t) ->
      let message = Format.sprintf  "Type is not comparable under constructor %s." s in
      let location = t.location in
      let content = make_content ~message ~location () in
      make ~stage ~content
    | `Self_mini_c_bad_capture (l, _t) ->
      let message = "Invalid capture." in
      let location = l in
      let content = make_content ~message ~location () in
      make ~stage ~content
