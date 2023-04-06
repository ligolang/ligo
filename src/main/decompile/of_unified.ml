open Helpers
open Ast_unified

let decompile (m : program) syntax : _ = specialise_and_print syntax m
let decompile_expression (e : expr) syntax : _ = specialise_and_print_expression syntax e
