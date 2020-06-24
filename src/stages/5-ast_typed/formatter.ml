open Display

let program_ppformat ~display_format f (typed,_) =
  match display_format with
  | Human_readable | Dev -> PP.program f typed

let program_jsonformat (typed,_) : json = PP_json.program typed

let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}
