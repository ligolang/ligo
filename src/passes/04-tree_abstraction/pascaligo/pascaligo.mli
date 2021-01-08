(** Converts PascaLIGO modules to the Simplified Abstract Syntax Tree. *)

module CST = Cst.Pascaligo
module AST = Ast_imperative
module Errors = Errors

module Decompiler = Decompiler

open Trace

(** Convert a concrete PascaLIGO expression CST to the imperative
    expression AST used by the compiler. *)
val compile_expression : CST.expr -> (AST.expr, Errors.abs_error) result

(** Convert a concrete PascaLIGO module CST to the miperative module
    AST used by the compiler. *)
val compile_module : CST.ast -> (AST.module_, Errors.abs_error) result

val decompile_expression : ?dialect:Decompiler.dialect -> AST.expr -> (CST.expr, _) result

val decompile_module : ?dialect:Decompiler.dialect -> AST.module_ -> (CST.ast, _) result
