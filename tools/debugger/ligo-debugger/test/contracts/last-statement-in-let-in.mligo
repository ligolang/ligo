[@entry]
let main () (s : int) : operation list * int =
  let a = s + 2 in
  let b = s - 2 in
  (([] : operation list), a + b)
