open Simple_utils.Display

let program_ppformat ~display_format f typed =
  match display_format with
  | Human_readable | Dev -> PP.program f typed

let program_jsonformat p : json =
  To_yojson.program p

let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}
