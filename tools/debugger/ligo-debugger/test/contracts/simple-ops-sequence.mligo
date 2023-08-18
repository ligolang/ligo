[@entry]
let main (_, s : unit * int) : operation list * int =
  let s2 = s + 1 in
  let s3 = s2 + 2 in
  let f (x : int) = x * x + 3 in let s5 = f s3 in
  let s6 = s5 * 2 in
  (([] : operation list), s6)
