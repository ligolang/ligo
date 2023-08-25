[@entry]
let main () (s : int) : operation list * int =
  let s2 = if s > 10 then s * 2 else s / 2 in
  (([] : operation list), s2)
