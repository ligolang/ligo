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
val compile_program    : raise:(Errors.abs_error list, Main_warnings.all) Simple_utils.Trace.raise -> CST.declarations -> AST.program

val decompile_expression : AST.expr -> CST.expr

val decompile_declarations : AST.program -> CST.declarations

val decompile_pattern_to_string : AST.type_expression option AST.Pattern.t -> string
