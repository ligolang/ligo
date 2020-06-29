open Trace
open Ast_imperative
open Helpers

let decompile (program : program) syntax : (_ , _) result =
  let%bind syntax = syntax_to_variant syntax None in
  specialise_and_print syntax program

let decompile_expression (e : expression) syntax : (_ , _) result =
  specialise_and_print_expression syntax e
