open Display

let program_ppformat_fully_typed ~display_format f typed =
  match display_format with
  | Human_readable | Dev -> PP.program_fully_typed f typed

let program_jsonformat_fully_typed p : json =
  To_yojson.program_fully_typed p

let program_ppformat_with_unification_vars ~display_format f typed =
  match display_format with
  | Human_readable | Dev -> PP.program_with_unification_vars f typed

let program_jsonformat_with_unification_vars p : json =
  To_yojson.program_with_unification_vars p

let program_format_fully_typed : 'a format = {
  pp = program_ppformat_fully_typed;
  to_json = program_jsonformat_fully_typed;
}

let program_format_with_unification_vars : 'a format = {
  pp = program_ppformat_with_unification_vars;
  to_json = program_jsonformat_with_unification_vars;
}
