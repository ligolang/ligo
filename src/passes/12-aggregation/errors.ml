open Simple_utils.Display

let stage = "aggregation"

type aggregation_error =
  [ `Aggregation_corner_case of string
  | `Aggregation_redundant_pattern of Location.t
  ]
[@@deriving poly_constructor { prefix = "aggregation_" }]

let error_ppformat
    :  display_format:string display_format -> Format.formatter -> aggregation_error
    -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Aggregation_corner_case desc ->
      Format.fprintf f "@[<hv>An aggregation corner case occurred:@.%s@]" desc
    | `Aggregation_redundant_pattern loc ->
      Format.fprintf f "@[<hv>%a@.Redundant pattern matching@]" Snippet.pp loc)


let error_json : aggregation_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Aggregation_corner_case desc ->
    let message =
      Format.sprintf "@[<hv>An aggregation corner case occurred:@.%s@]" desc
    in
    let content = make_content ~message () in
    make ~stage ~content
  | `Aggregation_redundant_pattern location ->
    let message = "Redundant pattern matching" in
    let content = make_content ~message ~location () in
    make ~stage ~content
