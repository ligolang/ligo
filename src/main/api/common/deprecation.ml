(* Temporary pieces of code to warn user on deprecated commands *)
open Simple_utils.Trace

let cameligo designation attr x =
  Format.asprintf
    {|
  %s declaration through CLI is deprecated.
  Please use annotations in the source:
  
  [@%s]
  let %s = ...
|}
    designation
    attr
    x


let jsligo designation attr x =
  Format.asprintf
    {|
  %s declaration through CLI is deprecated.
  Please use annotations in the source:
  
  @%s
  const %s = ...
|}
    designation
    attr
    x


let entry_cli ~raise syntax entries =
  let designation = "Entry-point" in
  let attr = "entry" in
  match entries with
  | hd :: _ ->
    let err =
      match syntax with
      | Syntax_types.CameLIGO -> cameligo designation attr hd
      | _ -> jsligo designation attr hd
    in
    raise.error (`Main_deprecated_views_cli err)
  | [] -> ()


let view_cli ~raise syntax views =
  let designation = "View" in
  let attr = "view" in
  match views with
  | hd :: _ ->
    let err =
      match syntax with
      | Syntax_types.CameLIGO -> cameligo designation attr hd
      | _ -> jsligo designation attr hd
    in
    raise.error (`Main_deprecated_views_cli err)
  | [] -> ()
