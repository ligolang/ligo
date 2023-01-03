open Simple_utils.Display

let ppx_ppformat ~display_format ~no_colour f buf =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
  | Human_readable | Dev -> Format.fprintf f "%s" (Buffer.contents buf)

let ppx_jsonformat buf : json =
  let s = Format.asprintf "%s" (Buffer.contents buf) in
  `String s

let ppx_format : 'a format = {
  pp = ppx_ppformat;
  to_json = ppx_jsonformat;
}
