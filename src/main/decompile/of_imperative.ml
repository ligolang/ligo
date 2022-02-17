open Ast_imperative
open Helpers

let decompile ~raise ?dialect (m : module_) syntax : _  =
  let syntax = Syntax.of_string_opt ~raise ?dialect syntax None in
  specialise_and_print syntax m

let decompile_expression (e : expression) syntax : _  =
  specialise_and_print_expression syntax e

let decompile_type_expression (e : type_expression) syntax : _  =
  specialise_and_print_type_expression syntax e
