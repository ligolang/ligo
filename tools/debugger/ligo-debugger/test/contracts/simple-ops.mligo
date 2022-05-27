let main (_, s : unit * int) : operation list * int =
  let s2 = s + 42 in
  let s3 = s2 * s2 * 2 in
  (([] : operation list), s2)
