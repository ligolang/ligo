let foo (type a) (el : a) = el

[@entry]
let main () (s : int) : operation list * int =
  let res = foo (s + s) in
  let ops = foo ([] : operation list) in
  (ops, res)
