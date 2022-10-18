open Simple_utils.Display

let pp_result_json ppf result =
  let open Tezos_utils.Michelson in
  let json = get_json result in
  Format.fprintf ppf "%a" Data_encoding.Json.pp json

let pp_hex ppf michelson =
  let hex = Proto_alpha_utils.Memory_proto_alpha.to_hex michelson in
  Format.fprintf ppf "%a" Hex.pp hex

let pp_comment ppf michelson =
  let open Tezos_micheline.Micheline_printer in
  let michelson = Tezos_micheline.Micheline.strip_locations michelson in
  let michelson = printable (fun prim -> prim) michelson in
  print_expr ppf michelson

let mich_value_jsonformat michelson_format code : json =
  match michelson_format with
  | `Text ->
    let code_as_str = Format.asprintf "%a" pp_comment code in
    `Assoc [("text_code" , `String code_as_str)]
  | `Hex ->
    let code_as_hex = Format.asprintf "%a" pp_hex code in
    `Assoc [("hex_code" , `String code_as_hex)]
  | `Json ->
    let code_as_str = Format.asprintf "%a" pp_result_json code in
    `Assoc [("json_code" , `String code_as_str)]

let mich_value_ppformat michelson_format ~display_format f code =
  let mich_pp = fun michelson_format ->  match michelson_format with
    | `Text -> pp_comment
    | `Json -> pp_result_json
    | `Hex -> pp_hex in
  match display_format with
   | Human_readable | Dev -> (
       let m = Format.asprintf "%a\n" (mich_pp michelson_format) code in
       Format.pp_print_string f m
     )

let mich_value_format michelson_format : 'a format = {
  pp = mich_value_ppformat michelson_format;
  to_json = mich_value_jsonformat michelson_format;
}

let tests_ppformat ~display_format f (print_values, toplevel_env) =
  let pp_result ppf (n, v) = Format.fprintf ppf "- %s exited with value %a." n PP.pp_value v in
  let pp_toplevel_env ppf lst = Format.fprintf ppf "@[<v>%a@]" (Simple_utils.PP_helpers.list_sep pp_result (Simple_utils.PP_helpers.tag "@.")) lst in
  match display_format with
  | Dev | Human_readable when print_values ->
    Format.fprintf f "@[<v>Everything at the top-level was executed.@.%a@]" pp_toplevel_env toplevel_env
  | _ -> ()


let tests_jsonformat b : json = ignore b ; `Null

let tests_format : 'a format = {
  pp = tests_ppformat;
  to_json = tests_jsonformat;
}
