open Display

let program_ppformat ~display_format (p,_) =
  match display_format with
  | Human_readable | Dev ->
    let buffer = Buffer.create 100 in
    let formatter = Format.formatter_of_buffer buffer in
    PP.program formatter p;
    Format.pp_print_flush formatter ();
    (Location.dummy, Buffer.contents buffer)

let program_jsonformat (p,_) : json =
  let p' = Format.asprintf "%a" PP.program p in
  `Assoc [("AST" , `String p')]

let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}