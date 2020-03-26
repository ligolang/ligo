open Types
open Combinators

let basic_int_quote_env : environment =
  let e = Environment.empty in
  Environment.add (Var.of_name "input", t_int) e
