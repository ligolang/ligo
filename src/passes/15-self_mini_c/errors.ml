open Simple_utils.Display
open Ligo_prim

let stage = "self_mini_c"

type self_mini_c_error = [
  | `Self_mini_c_bad_self_address of Constant.constant'
  | `Self_mini_c_not_a_function
  | `Self_mini_c_not_a_pair
  | `Self_mini_c_could_not_aggregate_entry
  | `Self_mini_c_corner_case of string
  | `Self_mini_c_fvs_in_create_contract_lambda of Mini_c.expression * Value_var.t
  | `Self_mini_c_create_contract_lambda of Constant.constant' * Mini_c.expression
] [@@deriving poly_constructor { prefix = "self_mini_c_" }]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_mini_c_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_mini_c_corner_case str ->
      Format.fprintf f "%s" str
    | `Self_mini_c_bad_self_address _cst ->
      let s = Format.asprintf "\"Tezos.self\" must be used directly and cannot be used via another function." in
      Format.pp_print_string f s ;
    | `Self_mini_c_not_a_function -> Format.fprintf f "Invalid type for entrypoint.@.An entrypoint must of type \"parameter * storage -> operation list * storage\"."
    | `Self_mini_c_not_a_pair -> Format.fprintf f "Invalid type for entrypoint.@.An entrypoint must of type \"a * storage -> b\"."
    | `Self_mini_c_could_not_aggregate_entry -> Format.fprintf f "Invalid type for entrypoint.@.An entrypoint must of type \"parameter * storage -> operation list * storage\"."
    | `Self_mini_c_fvs_in_create_contract_lambda (e,v) ->
      Format.fprintf f
        "@[<hv>%a@.Not all free variables could be inlined in Tezos.create_contract usage: %a.@]"
        Simple_utils.Snippet.pp e.location
        Value_var.pp v
    | `Self_mini_c_create_contract_lambda (_cst,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of Tezos.create_contract.@.The first argument must be an inline function. @]"
        Simple_utils.Snippet.pp e.location
  )

  let error_json : self_mini_c_error -> Error.t = fun e ->
    let open Error in
    match e with
    | `Self_mini_c_corner_case message ->
      let content = make_content ~message () in
      make ~stage ~content
    | `Self_mini_c_bad_self_address _ ->
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
  
let error_jsonformat : self_mini_c_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Self_mini_c_bad_self_address cst ->
    let msg = Format.asprintf "%a is only allowed at top-level" Constant.pp_constant' cst in
    let content = `Assoc [
      ("message", `String msg); ]
    in
    json_error ~stage ~content
  | `Self_mini_c_corner_case msg ->
    let content = `Assoc [
      ("message", `String msg); ]
    in
    json_error ~stage ~content
  | `Self_mini_c_not_a_function ->
    let content = `Assoc [
      ("message", `String "getting function has failed"); ]
    in
    json_error ~stage ~content
  | `Self_mini_c_not_a_pair ->
    let content = `Assoc [
      ("message", `String "getting pair has failed"); ]
    in
    json_error ~stage ~content
  | `Self_mini_c_could_not_aggregate_entry ->
    let content = `Assoc [
      ("message", `String "could not aggregate"); ]
    in
    json_error ~stage ~content
  | `Self_mini_c_fvs_in_create_contract_lambda (e, v) ->
    let loc = Value_var.get_location v in
    let message = `String "Free variables are not allowed in CREATE_CONTRACT lambdas" in
    let loc = `String (Format.asprintf "%a" Simple_utils.Location.pp loc) in
    let expression = `String (Format.asprintf "%a" Mini_c.PP.expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
    ] in
    json_error ~stage ~content
  | `Self_mini_c_create_contract_lambda (cst,e) ->
    let message = `String (Format.asprintf "First argument of %a must be inlined using a lambda" Mini_c.PP.constant cst) in
    let loc = `String (Format.asprintf "%a" Simple_utils.Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Mini_c.PP.expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
    ] in
    json_error ~stage ~content
