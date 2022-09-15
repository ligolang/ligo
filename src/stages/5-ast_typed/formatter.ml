open Simple_utils.Display

let program_ppformat ~display_format f typed =
  match display_format with
  | Human_readable | Dev -> PP.program ~use_hidden:true f typed

let program_jsonformat p : json =
  Types.program_to_yojson p

let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}
