open Simple_utils.Display

let stage = "self_ast_aggregated"

type self_ast_aggregated_error = [
  | `Self_ast_aggregated_expected_obj_ligo of Location.t
] [@@deriving poly_constructor { prefix = "self_ast_aggregated_" }]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_ast_aggregated_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_ast_aggregated_expected_obj_ligo loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid call to Test primitive.@]"
        Snippet.pp loc
  )

let error_jsonformat : self_ast_aggregated_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Self_ast_aggregated_expected_obj_ligo loc ->
    let message = `String "unexpected Test primitive" in
    let description = `String "these Test primitive or type cannot be used in code to be compiled or run" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("description", description);
       ]
    in
    json_error ~stage ~content
