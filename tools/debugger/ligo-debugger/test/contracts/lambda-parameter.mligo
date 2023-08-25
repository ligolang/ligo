[@entry]
let main (p : int * int -> int) (s : int) : operation list * int =
  (([] : operation list), p(s, s + 2))
