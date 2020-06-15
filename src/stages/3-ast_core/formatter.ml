open Display

let program_ppformat ~display_format f (p,_) =
  match display_format with
  | Human_readable | Dev -> PP.program f p

let program_jsonformat (p,_) : json =
  let s = Format.asprintf "%a" PP.program p in
  `Assoc [("Core(temp)" , `String s)]

let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}

let expression_ppformat ~display_format f (p,_) =
  match display_format with
  | Human_readable | Dev -> PP.expression f p

let expression_jsonformat (p,_) : json =
  let core' = Format.asprintf "%a" PP.expression p in
  `Assoc [("Core(temp)" , `String core')]

let expression_format : 'a format = {
  pp = expression_ppformat;
  to_json = expression_jsonformat;
}