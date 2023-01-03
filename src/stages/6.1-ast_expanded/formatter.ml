open Simple_utils.Display

let expression_ppformat ~display_format ~no_colour f typed =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
  | Human_readable | Dev -> PP.expression f typed


let expression_jsonformat p : json = Types.expression_to_yojson p

let expression_format : 'a format =
  { pp = expression_ppformat; to_json = expression_jsonformat }
