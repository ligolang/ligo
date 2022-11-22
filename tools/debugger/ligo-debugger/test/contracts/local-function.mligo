let main (_, s : unit * int) : operation list * int =
  let f (x : int) (y : int) : int = x + y in
  let a = 42 in
  let b = s + 2 in
  (([] : operation list), f a b)
