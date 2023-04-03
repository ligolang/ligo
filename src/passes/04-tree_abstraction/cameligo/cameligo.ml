module CST = Cst.Cameligo
module AST = Ast_imperative
module Compiler = Compiler
module Decompiler = Decompiler
module Errors = Errors

let compile_program = Compiler.compile_program
let compile_expression = Compiler.compile_expression
let decompile_program = Decompiler.decompile_program
let decompile_expression = Decompiler.decompile_expression

let decompile_pattern_to_string p =
  let p = Decompiler.decompile_pattern p in
  let p =
    Parsing.Cameligo.pretty_print_pattern
      ~cols:80
      Parsing.Cameligo.Pretty.default_environment
      p
  in
  Buffer.contents p


let decompile_type_expression_to_string p =
  let t = Decompiler.decompile_type_expr p in
  let t =
    Parsing.Cameligo.pretty_print_type_expr Parsing.Cameligo.Pretty.default_environment t
  in
  Buffer.contents t
