module Errors = Errors

let compile_expression ~raise : Ast_typed.expression -> Ast_aggregated.expression =
 fun x -> Compiler.compile_expression ~raise x


let compile_program ~raise
    : Ast_typed.expression -> Ast_typed.program -> Ast_aggregated.program
  =
 fun hole program ->
  let scope, deduplicate_scope, aliases, decls =
    Compiler.compile_program ~raise program
  in
  let init = Compiler.compile_expression ~raise ~scope ~deduplicate_scope ~aliases hole in
  decls, init


let decompile : Ast_aggregated.expression -> Ast_typed.expression = Decompiler.decompile
