let add (a, b : int * int) : int = a + b

let foo (a, b : int * int) : int = (a + b) * (a - b)

[@entry]
let main () (s : int) : operation list * int =
  let inner (a, b : int * int) : int = a * b in
  let s1 = add (s, s + 2) in
  let s2 = foo (s1, s) in
  let s3 = inner (s1, s2) in
  (([] : operation list), s + s1 + s2 + s3)
