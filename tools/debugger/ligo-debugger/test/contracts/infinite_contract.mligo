let rec infinite (a : int) : int =
  infinite (a + 1)

[@entry]
let main (_, s : unit * int) : operation list * int =
  let s1 = infinite (s) in
  (([] : operation list), s)
