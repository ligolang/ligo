open Simple_utils.Display

let stage = "purification"

type purification_error = [
  | `purification_corner_case of string
]

let corner_case s = `purification_corner_case s

let error_ppformat : display_format:string display_format ->
  purification_error -> Location.t * string =
  fun ~display_format a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `purification_corner_case s ->
      (Location.dummy, 
      Format.asprintf
        "@[<hv>Corner case: %s@]"
        s)
  )

let error_jsonformat : purification_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `purification_corner_case s ->
    let message = `String "corner case" in
    let content = `Assoc [
      ("message", message);
      ("value", `String s)
    ] in
    json_error ~stage ~content
