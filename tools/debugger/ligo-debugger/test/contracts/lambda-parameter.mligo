[@entry]
let main (p, s : (int * int -> int) * int) : operation list * int =
  (([] : operation list), p(s, s + 2))
