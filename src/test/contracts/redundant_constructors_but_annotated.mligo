type union_b =
| A of nat
| B of nat

type union_a =
| A of int
| B of int

let x : operation list * union_b = ([]: operation list) , A 1n
