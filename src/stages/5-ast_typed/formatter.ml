open Display

let program_ppformat ~display_format f (typed,_) =
  match display_format with
  | Human_readable | Dev -> PP.program f typed

let program_jsonformat (p,_) : json =
  let s = Format.asprintf "%a" PP.program p in
  `Assoc [("Typed(temp)" , `String s)]


let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}
