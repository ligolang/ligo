open Ppxlib

let ext extension_name_s =
  Extension.declare_with_path_arg
    extension_name_s
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg expr ->
       Ppx_let_expander.expand extension_name_s ~modul:arg expr)
;;

let () =
  Driver.register_transformation
    "let"
    ~extensions:(List.map ext [
      "bind";
      "xxx";
    ])
;;
