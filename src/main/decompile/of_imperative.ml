open Trace
open Ast_imperative
open Helpers

let decompile ?dialect (program : program) syntax : (_ , _) result =
  let%bind syntax = syntax_to_variant ?dialect syntax None in
  specialise_and_print syntax program

let decompile_expression (e : expression) syntax : (_ , _) result =
  specialise_and_print_expression syntax e
