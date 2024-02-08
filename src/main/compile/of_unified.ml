open Main_errors
open Simple_utils.Trace
open Ast_unified
open Nanopasses

let compile ~raise ~options (p : program) : Ast_core.program =
  trace ~raise nanopasses_tracer @@ compile_program ~options p


let compile_until ~raise ~options ?stop_before (p : program) : Ast_unified.program =
  trace ~raise nanopasses_tracer
  @@ execute_nanopasses ~options ~sort:Selector.program ?stop_before p


let compile_expression
    ~(raise : (Main_errors.all, Main_warnings.all) raise)
    ~options
    ?disable_initial_check
    (e : expr)
    : Ast_core.expression
  =
  trace ~raise nanopasses_tracer @@ compile_expression ~options ?disable_initial_check e


let compile_type_expression
    ~(raise : (Main_errors.all, Main_warnings.all) raise)
    ~options
    ?disable_initial_check
    (e : ty_expr)
    : Ast_core.type_expression
  =
  trace ~raise nanopasses_tracer
  @@ compile_type_expression ~options ?disable_initial_check e
