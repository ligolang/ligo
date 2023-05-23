open Simple_utils.Display

let program_ppformat ~show_loc ~display_format ~no_colour f p =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  ignore no_colour ;
  match display_format with
  | Human_readable | Dev -> PP.program ~show_loc f p


let program_format ~show_loc : 'a format =
  { pp = program_ppformat ~show_loc ; to_json = Yojson_conv.program_to_yojson }
