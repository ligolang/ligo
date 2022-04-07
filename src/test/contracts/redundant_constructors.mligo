type union_a =
| Add of int
| Remove of int

type union_b =
| Add of nat
| Remove of nat
| Config of nat

let x = Add 42n , (Add 42 : union_a)
