module Errors = Errors

let compile_expression ~raise : Ast_typed.expression -> Ast_aggregated.expression =
  fun x -> Compiler.compile_expression ~raise x

let compile_program : Ast_typed.expression -> Ast_typed.program -> Ast_aggregated.program =
  fun hole program ->
    let scope, deduplicate_scope, aliases, decls = Compiler.compile_program ~raise program in
    let init = Compiler.compile_expression ~raise ~scope ~deduplicate_scope ~aliases hole in
    (decls, init)

(* compile_expression_in_context [filler] [context] : let .. = .. in let .. = .. in [filler'] *)
let compile_expression_in_context : Ast_typed.expression -> Ast_typed.program -> Ast_aggregated.expression =
  fun hole program ->
    let decls, init = compile_program hole program in
    Ast_aggregated.context_apply decls init

let decompile = Decompiler.decompile
