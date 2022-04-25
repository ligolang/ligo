module CST = Cst.Pascaligo
module AST = Ast_imperative

module Compiler   = Compiler
module Decompiler = Decompiler
module Errors = Errors

let compile_declarations = Compiler.compile_declarations
let compile_expression = Compiler.compile_expression ~attr:[]

let decompile_declarations = Decompiler.decompile_declarations
let decompile_expression = Decompiler.decompile_expression
