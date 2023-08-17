(* Temporary pieces of code to warn user on deprecated commands *)
open Simple_utils.Trace

let view_cli ~raise syntax views =
  let cameligo x =
    Format.asprintf
      {|
    Views declaration through CLI is deprecated.
    Please use annotations in the source:
    
    [@view]
    let %s = ...
  |}
      x
  in
  let jsligo x =
    Format.asprintf
      {|
    Views declaration through CLI is deprecated.
    Please use annotations in the source:
    
    @view
    const %s = ...
  |}
      x
  in
  match views with
  | hd :: _ ->
    let err =
      match syntax with
      | Syntax_types.CameLIGO -> cameligo hd
      | _ -> jsligo hd
    in
    raise.error (`Main_deprecated_views_cli err)
  | [] -> ()
