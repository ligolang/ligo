module Display = Simple_utils.Display
module Ligo_Error = Simple_utils.Error

let graph_ppformat ~display_format ~no_colour f g =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
  | Display.Human_readable | Dev -> PP.graph f g

let graph_json g : Ligo_Error.t =
  let stage = "build system" in
  let message = Format.asprintf "%a" PP.graph g in
  let content = Ligo_Error.make_content ~message () in
  Ligo_Error.make ~stage ~content

let graph_jsonformat g : Yojson.Safe.t = Ligo_Error.to_yojson (graph_json g)
let graph_format : 'a Display.format = { pp = graph_ppformat; to_json = graph_jsonformat }
