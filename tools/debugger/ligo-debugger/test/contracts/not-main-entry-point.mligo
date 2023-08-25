[@entry]
let not_main () (s : int) : operation list * int =
  let s1 = s + 42 in
  (([] : operation list), s1)
