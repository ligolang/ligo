open Simple_utils.Display

let stage = "desugaring"

type desugaring_error = [ `Desugaring_corner_case of string ]
[@@deriving poly_constructor { prefix = "desugaring_" }]

let error_ppformat
    : display_format:string display_format -> Format.formatter -> desugaring_error -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Desugaring_corner_case s -> Format.fprintf f "@[<hv>Corner case: %s@]" s)


let error_json : desugaring_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Desugaring_corner_case e ->
    let message = Format.asprintf "Corner case: %s" e in
    let content = make_content ~message () in
    make ~stage ~content
