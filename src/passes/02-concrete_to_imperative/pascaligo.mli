(** Converts PascaLIGO programs to the Simplified Abstract Syntax Tree. *)

open Trace

module AST = Ast_imperative
module CST = Parser.Pascaligo.AST

(** Convert a concrete PascaLIGO expression AST to the imperative
    expression AST used by the compiler. *)
val compile_expression : CST.expr -> (AST.expr , Errors_pascaligo.abs_error) result

(** Convert a concrete PascaLIGO program AST to the miperative program
    AST used by the compiler. *)
val compile_program : CST.ast -> (AST.program, Errors_pascaligo.abs_error) result
