type storage = int

let main (ps: int * storage) =
  (([] : operation list) , ps.0 + ps.1)
