open Simple_utils.Display

let stage = "aggregation"

type aggregation_error = [
  | `Aggregation_corner_case of string
]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> aggregation_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Aggregation_corner_case desc ->
        Format.fprintf f
        "@[<hv>An aggregation corner case occurred:@.%s@]"
        desc
  )

let error_jsonformat : aggregation_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Aggregation_corner_case desc ->
    let message = `String desc in
    let content = `Assoc [
      ("message", message);
    ] in
    json_error ~stage ~content

