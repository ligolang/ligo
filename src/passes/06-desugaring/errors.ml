open Simple_utils.Display

let stage = "corification"

type purification_error = [ `Corification_corner_case of string ]
[@@deriving poly_constructor { prefix = "corification_" }]

let error_ppformat
    :  display_format:string display_format -> Format.formatter
    -> purification_error -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Corification_corner_case s ->
      Format.fprintf f "@[<hv>Corner case: %s@]" s)


let error_jsonformat : purification_error -> Yojson.Safe.t =
 fun a ->
  let json_error ~stage ~content =
    `Assoc
      [ "status", `String "error"; "stage", `String stage; "content", content ]
  in
  match a with
  | `Corification_corner_case s ->
    let message = `String "corner case" in
    let content = `Assoc [ "message", message; "value", `String s ] in
    json_error ~stage ~content
