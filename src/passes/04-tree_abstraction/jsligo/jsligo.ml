module CST = Cst.Jsligo
module AST = Ast_imperative

module Compiler   = Compiler
module Decompiler = Decompiler
module Errors = Errors

let compile_program     = Compiler.compile_program
let compile_expression = Compiler.compile_expression

let decompile_module    = Decompiler.decompile_module
let decompile_expression = Decompiler.decompile_expression

let decompile_pattern_to_string p = 
  let p = Decompiler.decompile_pattern p in
  let p = Parsing.Jsligo.pretty_print_pattern ~cols:80 p in
  Buffer.contents p