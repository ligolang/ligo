open Simple_utils.Display
open Simple_utils.Runned_result


let expression_ppformat ~display_format f runned_result =
  match display_format with
  | Human_readable | Dev -> (
    match runned_result with
    | Fail (fail_res : (int, string) Tezos_micheline.Micheline.node) ->
      Format.printf "failed with: %a" Tezos_utils.Michelson.pp fail_res
    | Success typed ->
      Ast_core.PP.expression f typed      
  )

let expression_jsonformat runned_result : json =
  match runned_result with
  | Fail fail_res ->
    let failstring = Format.asprintf "%a" Tezos_utils.Michelson.pp fail_res in
    `Assoc [("value", `Null) ; ("failure", `String failstring)]
  | Success typed ->
    `Assoc [("value", Ast_core.Yojson.expression typed) ; ("failure", `Null)]

let expression_format : 'a format = {
  pp = expression_ppformat ;
  to_json = expression_jsonformat ;
}
