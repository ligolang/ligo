let add (a, b : int * int) : int = a + b

let sub (a, b : int * int) : int = a - b

let main1 (_, s : unit * int) : operation list * int =
  (([] : operation list), add(s, s + 2))

let main2 (p, s : int * int) : operation list * int =
  (([] : operation list), sub(p, s))
