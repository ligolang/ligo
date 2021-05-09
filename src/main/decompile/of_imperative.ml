open Trace
open Ast_imperative
open Helpers

let decompile ?dialect (m : module_) syntax : (_ , _) result =
  let* syntax = syntax_to_variant ?dialect syntax None in
  specialise_and_print syntax m

let decompile_expression (e : expression) syntax : (_ , _) result =
  specialise_and_print_expression syntax e
