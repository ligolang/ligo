module CST = Cst.Pascaligo
module AST = Ast_imperative

module Compiler = Compiler
module Errors = Errors

let compile_program    = Compiler.compile_program
let compile_expression = Compiler.compile_expression
