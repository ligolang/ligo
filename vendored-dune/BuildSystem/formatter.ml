open Simple_utils.Display

let graph_ppformat ~display_format f g =
  match display_format with
  | Human_readable | Dev -> PP.graph f g

let graph_json g : Simple_utils.Error.t =
  let open Simple_utils.Error in
  let stage = "build system" in
  let message = Format.asprintf "%a" PP.graph g in
  let content = make_content ~message () in
  make ~stage ~content

let graph_jsonformat g : Yojson.Safe.t =
  Simple_utils.Error.to_yojson (graph_json g)

let graph_format : 'a format = {
  pp = graph_ppformat;
  to_json = graph_jsonformat;
}
