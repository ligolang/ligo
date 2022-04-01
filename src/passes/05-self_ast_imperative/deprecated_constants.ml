open Ast_imperative

let warn_constant name replacement l =
  `Self_ast_imperative_warning_deprecated_constant (l, name, replacement)

let warn ~add_warning : expression -> expression = fun e ->
  let () = match e.expression_content with
    | E_constant { cons_name = Deprecated { name ; const } ; _ } ->
       let replacement = try Some (Predefined.Tree_abstraction.pseudo_module_to_string const) with
                         | Failure _ -> None in
       add_warning (warn_constant name replacement e.location)
    | _ -> () in
  e
