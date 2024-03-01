open Simple_utils.Utils

let display_only_for_cameligo = "display-only-for-cameligo"
let display_only_for_jsligo = "display-only-for-jsligo"

(* If a comment starts with "display-only-for-cameligo" we don't threat it as
   a doc comment if target syntax is JsLIGO *)
let is_doc_comment : output_syntax:Syntax_types.t -> string -> bool =
 fun ~output_syntax path ->
  String.is_prefix ~prefix:"*" path
  &&
  let stripped =
    path
    |> String.chop_prefix_if_exists ~prefix:"*"
    |> String.strip ~drop:Char.is_whitespace
  in
  not
  @@ String.is_prefix
       stripped
       ~prefix:
         (match output_syntax with
         | CameLIGO -> display_only_for_jsligo
         | JsLIGO -> display_only_for_cameligo)


let format_doc_comment ~(source_syntax : Syntax_types.t) =
  (* we don't want to show "*" at hover, and for JsLIGO we strip "*" from every
     string of a comment, to match the TypeDoc's behaviour *)
  let strip_spaces_and_star =
    String.strip ~drop:Char.is_whitespace
    <@ String.chop_prefix_if_exists ~prefix:"*"
    <@ String.strip ~drop:Char.is_whitespace
  in
  let strip_internal_prefix =
    String.strip ~drop:Char.is_whitespace
    <@ String.chop_prefix_if_exists ~prefix:display_only_for_cameligo
    <@ String.chop_prefix_if_exists ~prefix:display_only_for_jsligo
  in
  strip_internal_prefix
  <@
  match source_syntax with
  | CameLIGO -> strip_spaces_and_star
  | JsLIGO ->
    String.strip ~drop:Char.is_whitespace
    <@ String.concat ~sep:"\n"
    <@ List.map ~f:strip_spaces_and_star
    <@ String.split_lines
