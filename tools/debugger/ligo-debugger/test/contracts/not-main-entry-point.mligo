let not_main (_, s : unit * int) : operation list * int =
  let s1 = s + 42 in
  (([] : operation list), s1)
