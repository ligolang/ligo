open Errors
open Ast_imperative
open Trace
open Stage_common.Helpers

let is_layout attr =
  if String.length attr > 7 && String.sub attr 0 7 = "layout:" then
    Some (String.sub attr 7 ((String.length attr)-7))
  else None

let layout_type_expression : type_expression -> (type_expression , self_ast_imperative_error) result = fun e ->
  let return type_content = ok {type_content; location=e.location } in
  match e.type_content with
  | T_sum cmap ->
     let* _ = bind_map_lmapi
       (fun k ({attributes;_} : _ Ast_imperative.row_element) ->
         if attributes |> List.map ~f:is_layout |> List.exists ~f:Option.is_some then
           update_annotation (warn_layout e.location k) @@
             ok ()
         else ok ()
       )
      cmap.fields in
    ok e
  | e -> return e
