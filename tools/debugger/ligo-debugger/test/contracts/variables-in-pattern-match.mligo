let pairFunc (a : int) : int * int = (a + 2, a - 2)

[@entry]
let main () (s : int) : operation list * int =
  let (a, b) = pairFunc s in
  (([] : operation list), a + b)
