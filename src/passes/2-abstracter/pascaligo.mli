(** Converts PascaLIGO programs to the Simplified Abstract Syntax Tree. *)

open Trace
open Ast_imperative

module Raw = Parser.Pascaligo.AST
module SMap = Map.String

(** Convert a concrete PascaLIGO expression AST to the simplified
    expression AST used by the compiler. *)
val abstr_expression : Raw.expr -> expr result

(** Convert a concrete PascaLIGO program AST to the simplified program
    AST used by the compiler. *)
val abstr_program : Raw.ast -> program result
