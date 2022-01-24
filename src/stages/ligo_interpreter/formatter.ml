open Simple_utils.Display

let test_ppformat ~display_format f v =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "Test passed with %a" PP.pp_value v

let test_jsonformat b : json = ignore b ; `Null

let test_format : 'a format = {
  pp = test_ppformat;
  to_json = test_jsonformat;
}

let tests_ppformat ~display_format f (toplevel_env) =
  let pp_result ppf (n, v) = Format.fprintf ppf "- %s exited with value %a." n PP.pp_value v in
  let pp_toplevel_env ppf lst = Format.fprintf ppf "@[<v>%a@]" (Simple_utils.PP_helpers.list_sep pp_result (Simple_utils.PP_helpers.tag "@.")) lst in
  match display_format with
  | Dev | Human_readable ->
     Format.fprintf f "@[<v>Everything at the top-level was executed.@.%a@]" pp_toplevel_env toplevel_env


let tests_jsonformat b : json = ignore b ; `Null

let tests_format : 'a format = {
  pp = tests_ppformat;
  to_json = tests_jsonformat;
}
