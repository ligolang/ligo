open Helpers
open Ast_imperative

let decompile (m : program) syntax : _ = specialise_and_print syntax m

let decompile_expression (e : expression) syntax : _ =
  specialise_and_print_expression syntax e
