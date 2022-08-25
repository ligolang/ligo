open Ast_imperative
open Helpers

let decompile ~raise (m : module_) syntax : _  =
  let syntax = Syntax.of_string_opt ~raise syntax None in
  specialise_and_print syntax m

let decompile_expression (e : expression) syntax : _  =
  specialise_and_print_expression syntax e
