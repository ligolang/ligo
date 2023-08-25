[@entry]
let main (p : int) (_ : bool) : operation list * bool =
  (([] : operation list), p > 0)
