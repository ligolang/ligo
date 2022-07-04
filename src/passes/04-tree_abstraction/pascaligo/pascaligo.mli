(** Converts PascaLIGO modules to the Simplified Abstract Syntax Tree. *)

module CST = Cst.Pascaligo
module AST = Ast_imperative
module Errors = Errors

module Decompiler = Decompiler

(** Convert a concrete PascaLIGO expression CST to the imperative
    expression AST used by the compiler. *)
val compile_expression : raise:(Errors.abs_error, Main_warnings.all) Simple_utils.Trace.raise -> CST.expr -> AST.expr

(** Convert a concrete PascaLIGO module CST to the miperative module
    AST used by the compiler. *)
val compile_program    : raise:(Errors.abs_error list, Main_warnings.all) Simple_utils.Trace.raise -> CST.declarations -> AST.module_

val decompile_expression : ?dialect:Decompiler.dialect -> AST.expr -> CST.expr

val decompile_declarations : ?dialect:Decompiler.dialect -> AST.module_ -> CST.declarations
