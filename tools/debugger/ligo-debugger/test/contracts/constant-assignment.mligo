let main (_, s : unit * int) : operation list * int =
  let s1 = 5 in
  (([] : operation list), s + s1)
