let add (a : int) (b, c : int * int) : int = a + b + c

let add5 : (int * int) -> int = add 5

let main (_, s : unit * int) : operation list * int =
  let a = s + 3 in
  let res = add5(a, s) in
  (([] : operation list), res)
