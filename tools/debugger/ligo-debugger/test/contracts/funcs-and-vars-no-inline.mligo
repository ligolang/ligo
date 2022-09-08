let func (a, b : int * int) : int =
  let c = a + b in
  let d = a - b in
  c * d

let main (_, s : unit * int) : operation list * int =
  let s1 = func (s, s + 2) in
  let s2 = 15 in
  (([] : operation list), s1 + s2)
