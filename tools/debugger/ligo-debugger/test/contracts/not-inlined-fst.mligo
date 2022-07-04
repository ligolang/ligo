let fst (p : int * int) : int = p.0

let main (_, s : unit * int) : operation list * int =
  let pair1 = (1, s) in
  let pair2 = (3, s) in
  (([] : operation list), s + fst(pair1) + fst(pair2))
