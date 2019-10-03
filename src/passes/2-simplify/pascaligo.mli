(** Converts PascaLIGO programs to the Simplified Abstract Syntax Tree. *)

open Ast_simplified

module Raw = Parser.Pascaligo.AST
module SMap = Map.String

(** Convert a concrete PascaLIGO expression AST to the simplified expression AST 
    used by the compiler. *)
val simpl_expression : Raw.expr -> expression Trace.result

(** Convert a concrete PascaLIGO program AST to the simplified program AST used 
    by the compiler. *)
val simpl_program : Raw.ast -> program Trace.result
