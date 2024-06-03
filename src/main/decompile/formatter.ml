module Display = Simple_utils.Display
module Runned_result = Simple_utils.Runned_result

let expression_ppformat ~display_format ~no_colour f runned_result =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
  | Display.Human_readable | Dev ->
    let open Runned_result in
    (match runned_result with
    | Fail (fail_res : (int, string) Tezos_micheline.Micheline.node) ->
      Format.printf "failed with: %a" Tezos_utils.Michelson.pp fail_res
    | Success typed -> Ast_core.PP.expression f typed)


let expression_jsonformat runned_result : Display.json =
  let open Runned_result in
  match runned_result with
  | Fail fail_res ->
    let failstring = Format.asprintf "%a" Tezos_utils.Michelson.pp fail_res in
    `Assoc [ "value", `Null; "failure", `String failstring ]
  | Success typed ->
    `Assoc [ "value", Ast_core.expression_to_yojson typed; "failure", `Null ]


let expression_format : 'a Display.format =
  { pp = expression_ppformat; to_json = expression_jsonformat }
