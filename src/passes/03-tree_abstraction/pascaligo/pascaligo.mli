(** Converts PascaLIGO programs to the Simplified Abstract Syntax Tree. *)

module CST = Cst.Pascaligo
module AST = Ast_imperative
module Errors = Errors

open Trace

(** Convert a concrete PascaLIGO expression CST to the imperative
    expression AST used by the compiler. *)
val compile_expression : CST.expr -> (AST.expr, Errors.abs_error) result

(** Convert a concrete PascaLIGO program CST to the miperative program
    AST used by the compiler. *)
val compile_program : CST.ast -> (AST.program, Errors.abs_error) result

val decompile_expression : AST.expr -> (CST.expr, _) result

val decompile_program : AST.program -> (CST.ast, _) result
