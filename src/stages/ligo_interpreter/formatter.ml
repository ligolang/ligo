open Display

let test_ppformat ~display_format f success =
  match display_format with
  | Human_readable | Dev ->
    Format.pp_print_string f (
      if success then "Test was successful"
      else "Test failed"
    )

let test_jsonformat b : json = `Bool b

let test_format : 'a format = {
  pp = test_ppformat;
  to_json = test_jsonformat;
}
