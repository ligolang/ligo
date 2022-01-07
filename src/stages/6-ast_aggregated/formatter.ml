open Simple_utils.Display

let expression_ppformat ~display_format f typed =
  match display_format with
  | Human_readable | Dev -> PP.expression f typed

let expression_jsonformat p : json =
  To_yojson.expression p
let expression_format : 'a format = {
  pp = expression_ppformat;
  to_json = expression_jsonformat;
}
