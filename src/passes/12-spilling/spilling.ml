module AST         = Ast_typed
module Append_tree = Simple_utils.Tree.Append

module Compiler   = Compiler
module Decompiler = Decompiler
module Errors = Errors

let compile_program    = Compiler.compile_program
let compile_expression = Compiler.compile_expression
let compile_type       = Compiler.compile_type

let decompile = Decompiler.decompile
