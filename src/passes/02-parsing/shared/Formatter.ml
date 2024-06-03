module Display = Simple_utils.Display

let ppx_ppformat ~display_format ~no_colour f buf =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
    Display.Human_readable | Dev ->
      Format.fprintf f "%s" (Buffer.contents buf)

let ppx_jsonformat buf : Display.json =
  `String (Format.asprintf "%s" (Buffer.contents buf))

let ppx_format : 'a Display.format = {
  pp = ppx_ppformat;
  to_json = ppx_jsonformat;
}
