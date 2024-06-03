module Display = Simple_utils.Display

let program_ppformat ~display_format ~no_colour f p =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
  | Display.Human_readable | Dev -> PP.program f p


let program_jsonformat p : Display.json = Types.program_to_yojson p

let program_format : 'a Display.format =
  { pp = program_ppformat; to_json = program_jsonformat }
