open Ast_simplified
open Trace

let compile_contract_entry (program : program) entry_point =
  let%bind (prog_typed , state) = Typer.type_program program in
  let () = Typer.Solver.discard_state state in
  Of_typed.compile_contract_entry prog_typed entry_point

let compile_function_entry (program : program) entry_point : _ result =
  let%bind (prog_typed , state) = Typer.type_program program in
  let () = Typer.Solver.discard_state state in
  Of_typed.compile_function_entry prog_typed entry_point

let compile_expression_as_function_entry (program : program) entry_point : _ result =
  let%bind (typed_program , state) = Typer.type_program program in
  let () = Typer.Solver.discard_state state in
  Of_typed.compile_expression_as_function_entry typed_program entry_point

(* TODO: do we need to thread the state here? Also, make the state arg. optional. *)
let compile_expression_as_function ?(env = Ast_typed.Environment.full_empty) ~(state : Typer.Solver.state) (ae : Ast_simplified.expression) : _ result =
  let%bind (typed , state) = Typer.type_expression env state ae in
  (* TODO: move this to typer.ml *)
  let typed =
    if false then
      let () = failwith "TODO : subst all" in let _todo = ignore (env, state) in typed
    else
      typed
  in
  Of_typed.compile_expression_as_function typed

let uncompile_typed_program_entry_expression_result program entry ex_ty_value =
  let%bind output_type =
    let%bind entry_expression = Ast_typed.get_entry program entry in
    ok entry_expression.type_annotation
  in
  let%bind typed = Of_typed.uncompile_value ex_ty_value output_type in
  Typer.untype_expression typed

let uncompile_typed_program_entry_function_result program entry ex_ty_value =
  let%bind output_type =
    let%bind entry_expression = Ast_typed.get_entry program entry in
    let%bind (_ , output_type) = Ast_typed.get_t_function entry_expression.type_annotation in
    ok output_type
  in
  let%bind typed = Of_typed.uncompile_value ex_ty_value output_type in
  Typer.untype_expression typed
