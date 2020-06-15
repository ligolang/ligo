open Trace
open Ast_sugar
open Sugar_to_core
open Main_errors

type form = 
  | Contract of string
  | Env

let compile (program : program) : (Ast_core.program , _) result =
  trace sugar_to_core_tracer @@ compile_program program

let compile_expression (e : expression) : (Ast_core.expression , _) result =
  trace sugar_to_core_tracer @@ compile_expression e

let list_declarations (program : program) : string list =
  List.fold_left
    (fun prev el -> 
      let open Location in
      match el.wrap_content with
      | Declaration_constant (var,_,_,_) -> (Var.to_name var)::prev
      | _ -> prev) 
    [] program
