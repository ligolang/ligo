[@entry]
let main (_, s : unit * int) : operation list * int =
  let r = { a = 42; b = 0n; c = "!" } in
  (([] : operation list), s + r.a)
