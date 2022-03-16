open Ast_sugar
open Desugaring

let compile ~raise (p : module_) : Ast_core.module_  =
  let p = compile_module p in
  Self_ast_core.all_module ~raise p

let compile_expression ~raise (e : expression) : Ast_core.expression  =
  let e = compile_expression e in
  Self_ast_core.all_expression ~raise e


let list_declarations (m : module_) : expression_variable list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match el.wrap_content with
      | Declaration_constant {binder;_} -> binder.var::prev
      | _ -> prev)
    ~init:[] m
