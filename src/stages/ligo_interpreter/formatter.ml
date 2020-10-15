open Display

let program_ppformat ~display_format f program_as_string =
  match display_format with
  | Human_readable | Dev ->
    Format.pp_print_string f program_as_string

let program_jsonformat program_as_string : json =
  let s = Format.asprintf "%s" program_as_string in
  `Assoc [("Typed(temp)" , `String s)]

let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}
