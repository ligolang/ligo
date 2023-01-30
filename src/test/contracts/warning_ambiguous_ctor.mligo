type union_a =
| A of int
| B of int

type union_b =
| A of int
| B of nat
(* here we expect a warning because both A constructor have the same parameter type *)
let main = fun (() : unit) (_: union_b) -> ([]: operation list) , A 1
