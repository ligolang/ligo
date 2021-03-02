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
