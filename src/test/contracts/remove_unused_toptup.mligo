let (x, y) = (1n, 2n)

let main (_ : unit) (s : int) : operation list * int =
  (([] : operation list), s + x + y)
