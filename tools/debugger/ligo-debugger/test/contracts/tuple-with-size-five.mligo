[@entry]
let main () (s : int) : operation list * int =
  let (a, b, c, d, e) = (1, 2, 3, 4, 5) in
  (([] : operation list), s + a + b + c + d + e)
