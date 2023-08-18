[@entry]
let main (_, s : unit * int) : operation list * int =
  let a = if s > 10 then s - 10 else s + 10 in
  (([] : operation list), a)
