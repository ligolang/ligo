let applyOp (f : int -> int -> int) (a, b : int * int) =
  let partApplied = f a in
  let fullyApplied = partApplied (b + b) in
  fullyApplied

let add (a : int) (b : int) = a + b

let sub (a : int) (b : int) = a - b

[@entry]
let main (_, s : unit * int) : operation list * int =
  let f1 = applyOp add in
  let f2 = applyOp sub in
  (([] : operation list), (applyOp add (s, s + 2)) * (applyOp sub (s, s - 4)))
