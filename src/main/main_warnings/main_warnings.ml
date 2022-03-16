open Simple_utils
open Display

type all =
[
  | `Self_ast_typed_warning_unused of Location.t * string
  | `Self_ast_typed_warning_muchused of Location.t * string
  | `Self_ast_imperative_warning_layout of (Location.t * Ast_imperative.label)
  | `Self_ast_imperative_warning_deprecated_constant of Location.t * string * string option
  | `Main_view_ignored of Location.t
  | `Pascaligo_deprecated_case of Location.t
  | `Pascaligo_deprecated_semi_before_else of Location.t
]

let warn_layout loc lab = `Self_ast_imperative_warning_layout (loc,lab)

let pascaligo_deprecated_case loc = `Pascaligo_deprecated_case loc

let pascaligo_deprecated_semi_before_else loc = `Pascaligo_deprecated_semi_before_else loc

let pp : display_format:string display_format ->
  Format.formatter -> all -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Main_view_ignored loc ->
      Format.fprintf f "@[<hv>%a@.This view will be ignored, command line option override [@ view] annotation@.@]"
      Snippet.pp loc
    | `Pascaligo_deprecated_case loc ->
      Format.fprintf f "@[<hv>%a@.Deprecated syntax behind `of`. An opening bracket `[` is expected behind `of` and `end` is expected to be replaced with a closing bracket `]`.@.@]"
      Snippet.pp loc
    | `Pascaligo_deprecated_semi_before_else loc ->
      Format.fprintf f "@[<hv>%a@.Deprecated semicolon `;` before the `else` keyword. Please remove the semicolon.@.@]"
      Snippet.pp loc
    | `Self_ast_typed_warning_unused (loc, s) ->
        Format.fprintf f
          "@[<hv>%a:@.Warning: unused variable \"%s\".@.Hint: replace it by \"_%s\" to prevent this warning.\n@]"
          Snippet.pp loc s s
    | `Self_ast_typed_warning_muchused (loc, s) ->
        Format.fprintf f
          "@[<hv>%a:@.Warning: variable \"%s\" cannot be used more than once.\n@]"
          Snippet.pp loc s
    | `Self_ast_imperative_warning_layout (loc,Label s) ->
        Format.fprintf f
          "@[<hv>%a@ Warning: layout attribute only applying to %s, probably ignored.@.@]"
          Snippet.pp loc s
    | `Self_ast_imperative_warning_deprecated_constant (loc, name, replacement) ->
        Format.fprintf f
          "@[<hv>%a@ Warning: constant %s is being deprecated soon.%s@.@]"
          Snippet.pp loc name (match replacement with
                               | Some r -> Format.asprintf " Consider using %s instead." r
                               | None -> "")
  )
let to_json : all -> Yojson.Safe.t = fun a ->
  let json_warning ~stage ~content =
    `Assoc [
      ("status", `String "warning") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Main_view_ignored loc ->
    let message = `String "command line option overwrites annotated views" in
    let stage   = "Main" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
                      ("message", message);
                      ("location", loc);
                    ] in
    json_warning ~stage ~content
  | `Pascaligo_deprecated_case loc ->
    let message = `String "deprecated case syntax" in
    let stage   = "lexer" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
                      ("message", message);
                      ("location", loc);
                    ] in
    json_warning ~stage ~content
  | `Pascaligo_deprecated_semi_before_else loc ->
    let message = `String "deprecated semicolon before else" in
    let stage   = "lexer" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
                      ("message", message);
                      ("location", loc);
                    ] in
    json_warning ~stage ~content
  | `Self_ast_typed_warning_unused (loc, s) ->
     let message = `String "unused variable" in
     let stage   = "self_ast_typed" in
     let description = `String s in
     let loc = `String (Format.asprintf "%a" Location.pp loc) in
     let content = `Assoc [
                       ("message", message);
                       ("location", loc);
                       ("variable", description)
                     ] in
     json_warning ~stage ~content
  | `Self_ast_typed_warning_muchused (loc, s) ->
     let message = `String "much used variable" in
     let stage   = "self_ast_typed" in
     let description = `String s in
     let loc = `String (Format.asprintf "%a" Location.pp loc) in
     let content = `Assoc [
                       ("message", message);
                       ("location", loc);
                       ("variable", description)
                     ] in
     json_warning ~stage ~content
  | `Self_ast_imperative_warning_layout (loc, s) ->
    let message = `String (Format.asprintf "Layout attribute on constructor %a" Ast_imperative.PP.label s) in
     let stage   = "self_ast_imperative" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_warning ~stage ~content
  | `Self_ast_imperative_warning_deprecated_constant (loc, name, _) ->
    let message = `String (Format.asprintf "Deprecated constant %s" name) in
     let stage   = "self_ast_imperative" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_warning ~stage ~content

let format = {pp;to_json}
