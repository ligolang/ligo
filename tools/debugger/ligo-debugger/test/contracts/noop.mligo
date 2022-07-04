let main (_, s : unit * int) : operation list * int =
  let s2 = s + 42 in
  (([] : operation list), s2)
