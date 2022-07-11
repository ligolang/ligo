open Ast_imperative

let is_layout attr = String.chop_prefix ~prefix:"layout:" attr

let layout_type_expression ~raise : type_expression -> type_expression  = fun e ->
  let return type_content = {type_content; location=e.location } in
  match e.type_content with
  | T_sum cmap ->
     let _ : unit list = List.map
       ~f:(fun (label,{attributes;_})->
         if attributes |> List.map ~f:is_layout |> List.exists ~f:Option.is_some then
            let () = raise.Simple_utils.Trace.warning @@ Main_warnings.warn_layout e.location label in
             ()
         else ()
       )
      cmap.fields in
    e
  | e -> return e
