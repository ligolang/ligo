open Types
open Combinators

let basic_int_quote_env : environment =
  let e = Environment.empty in
  Environment.add ("input", t_int) e
