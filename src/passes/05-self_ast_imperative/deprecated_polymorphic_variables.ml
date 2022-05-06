open Ast_imperative

let warn_constant var l =
  `Self_ast_imperative_warning_deprecated_polymorphic_variable (l, var)

let warn ~add_warning : type_expression -> type_expression = fun e ->
  let () = match e.type_content with
    | T_variable var when TypeVar.is_generalizable var ->
       add_warning (warn_constant var e.location)
    | _ -> () in
  e
