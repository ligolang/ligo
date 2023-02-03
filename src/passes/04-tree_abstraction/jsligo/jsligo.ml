module CST = Cst.Jsligo
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
  match p with
  | Ok p ->
    let p = Parsing.Jsligo.pretty_print_pattern ~cols:80 p in
    Buffer.contents p
  | Error e -> e


let decompile_type_expression_to_string t =
  let t = Decompiler.decompile_type_expr t in
  let t = Parsing.Jsligo.pretty_print_type_expr t in
  Buffer.contents t
