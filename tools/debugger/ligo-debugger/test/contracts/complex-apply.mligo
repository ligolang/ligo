let myFunc (a, b : int * int) =
  let add (a : int) (b, c : int * int) = a + b + c in
  let add5 = add 5 in
  add5(a, b)

[@entry]
let main (_, s : unit * int) : operation list * int =
  (([] : operation list), myFunc(s, s + 2))
