open Simple_utils.Display
open Simple_utils.Runned_result

let expression_ppformat ~display_format ~no_colour f runned_result =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
  | Human_readable | Dev ->
    (match runned_result with
    | Fail (fail_res : (int, string) Tezos_micheline.Micheline.node) ->
      Format.printf "failed with: %a" Tezos_utils.Michelson.pp fail_res
    | Success typed -> Ast_core.PP.expression f typed)


let expression_jsonformat runned_result : json =
  match runned_result with
  | Fail fail_res ->
    let failstring = Format.asprintf "%a" Tezos_utils.Michelson.pp fail_res in
    `Assoc [ "value", `Null; "failure", `String failstring ]
  | Success typed ->
    `Assoc [ "value", Ast_core.expression_to_yojson typed; "failure", `Null ]


let expression_format : 'a format =
  { pp = expression_ppformat; to_json = expression_jsonformat }
