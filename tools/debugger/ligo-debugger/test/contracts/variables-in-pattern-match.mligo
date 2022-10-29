let pairFunc (a : int) : int * int = (a + 2, a - 2)

let main (_, s : unit * int) : operation list * int =
  let (a, b) = pairFunc s in
  (([] : operation list), a + b)
