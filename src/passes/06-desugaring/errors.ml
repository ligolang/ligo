open Simple_utils.Display

let stage = "desugaring"

type desugaring_error = [ `Desugaring_corner_case of string ]
[@@deriving poly_constructor { prefix = "desugaring_" }]

let error_ppformat
    :  display_format:string display_format -> Format.formatter
    -> desugaring_error -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Desugaring_corner_case s -> Format.fprintf f "@[<hv>Corner case: %s@]" s)


let rec error_json : desugaring_error -> Ligo_prim.Error.t =
 fun e ->
  let open Ligo_prim.Error in
  match e with
  | `Desugaring_corner_case e ->
    let message = Format.asprintf "Corner case: %s" e in
    let content = make_content ~message () in
    make ~stage ~content

let error_jsonformat : desugaring_error -> Yojson.Safe.t =
 fun a ->
  let json_error ~stage ~content =
    `Assoc
      [ "status", `String "error"; "stage", `String stage; "content", content ]
  in
  match a with
  | `Desugaring_corner_case s ->
    let message = `String "corner case" in
    let content = `Assoc [ "message", message; "value", `String s ] in
    json_error ~stage ~content
