open Main_errors
open Trace
open Ast_imperative
open Purification

let compile (m : module_) : (Ast_sugar.module_, _) result =
  trace purification_tracer @@ compile_module m

let compile_expression (e : expression) : (Ast_sugar.expression , _) result =
  trace purification_tracer @@ compile_expression e

let pretty_print formatter (m : module_) =
  PP.module_ formatter m

let list_declarations (m : module_) : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match el.wrap_content with
      | Declaration_constant {binder;_} -> (Var.to_name binder.var.wrap_content)::prev
      | _ -> prev)
    ~init:[] m
