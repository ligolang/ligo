module Decompiler = Decompiler
module Program = Compiler_program
module Type = Compiler_type
module Errors = Errors

type compiled_expression = Program.compiled_expression

let compile_function   = Program.translate_function_body
let compile_expression = Program.translate_expression
let decompile_value    = Decompiler.decompile_value
