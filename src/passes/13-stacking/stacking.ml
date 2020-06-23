module Decompiler = Decompiler
module Program = Compiler_program
module Type = Compiler_type
module Environment = Compiler_environment
module Errors = Errors

include Program

let decompile_value = Decompiler.decompile_value
