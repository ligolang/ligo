open Display

let test_ppformat ~display_format f v =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "Test passed with %a" PP.pp_value v

let test_jsonformat b : json = ignore b ; `Null

let test_format : 'a format = {
  pp = test_ppformat;
  to_json = test_jsonformat;
}

let tests_ppformat ~display_format f v =
  match display_format with
  | Human_readable | Dev ->
     let pp_result ppf (n, v) = Format.fprintf ppf "- %s exited with value %a." n PP.pp_value v in
     let pp_scopes ppf = List.iter ~f: (Format.fprintf ppf "@[<v>@.%a@]" pp_result) in
     Format.fprintf f "@[<v>Everything at the top-level was executed.%a@]" pp_scopes v

let tests_jsonformat b : json = ignore b ; `Null

let tests_format : 'a format = {
  pp = tests_ppformat;
  to_json = tests_jsonformat;
}
