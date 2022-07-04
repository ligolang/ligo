module CST = Cst.Pascaligo
module AST = Ast_imperative

module Compiler   = Compiler
module Decompiler = Decompiler
module Errors = Errors

let compile_program = Compiler.compile_program
let compile_expression = Compiler.compile_expression ~attr:[]

let decompile_declarations = Decompiler.decompile_declarations
let decompile_expression = Decompiler.decompile_expression
