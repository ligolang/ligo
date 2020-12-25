open Main_errors
open Trace
open Ast_imperative
open Purification

let compile (program : program) : (Ast_sugar.program, _) result =
  trace purification_tracer @@ compile_program program

let compile_expression (e : expression) : (Ast_sugar.expression , _) result =
  trace purification_tracer @@ compile_expression e

let pretty_print formatter (program : program) = 
  PP.program formatter program

let list_declarations (program : program) : string list =
  List.fold_left
    (fun prev el -> 
      let open Location in
      match el.wrap_content with
      | Declaration_constant {binder;_} -> (Var.to_name binder.var.wrap_content)::prev
      | _ -> prev) 
    [] program
