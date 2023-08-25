[@entry]
let main () (s : int) : operation list * int =
  let s2 = s + 42 in
  (([] : operation list), s2)
