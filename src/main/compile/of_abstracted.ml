open Trace
open Ast_imperative
open Instruction_remover

type form = 
  | Contract of string
  | Env

let compile (program : program) : Ast_complex.program result =
  remove_instruction_in_program program

let compile_expression (e : expression) : Ast_complex.expression result =
  remove_instruction_in_expression e

let pretty_print formatter (program : program) = 
  PP.program formatter program

let list_declarations (program : program) : string list =
  List.fold_left
    (fun prev el -> 
      let open Location in
      match el.wrap_content with
      | Declaration_constant (var,_,_,_) -> (Var.to_name var)::prev
      | _ -> prev) 
    [] program
