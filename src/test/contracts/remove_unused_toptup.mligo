let (x, y) = (1n, 2n)

[@entry]
let main (_ : unit) (s : int) : operation list * int =
  (([] : operation list), s + x + y)
