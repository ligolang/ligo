[@entry]
let main (_, s : unit * int) : operation list * int =
  let foo (a, b : int * int) : int = a + b in
  let s1 = foo(s, s + 2) in
  let bar (a : int) (b : int) : int = a - b in
  let s2 = bar s (s - 2) in
  let baz (a : int) (b, c : int * int) : int = a + b * c in
  let s3 = baz s1 (s2, s) in
  (([] : operation list), s1 + s2 + s3)
