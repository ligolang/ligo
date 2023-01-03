open Simple_utils.Display

let program_ppformat ~display_format ~no_colour f typed =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
  | Human_readable | Dev -> PP.program ~use_hidden:true f typed


let program_jsonformat p : json = Types.program_to_yojson p
let program_format : 'a format = { pp = program_ppformat; to_json = program_jsonformat }
