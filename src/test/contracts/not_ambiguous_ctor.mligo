type union_a =
| A of int
| B of int

type union_b =
| A of nat
| B of nat

(* no warning is expected here since the constructor parameter of A in union_b do not match the one of union_a *)

[@entry]
let main = fun () (_ : union_b) -> ([] : operation list), A 1n
