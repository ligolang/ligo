open Display

let program_ppformat ~display_format f p =
  match display_format with
  | Human_readable | Dev -> PP.program f p

let program_jsonformat p : json =
  To_yojson.program p

let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}
