let main (_, s : unit * int) : operation list * int =
  let unused = () in
  (([] : operation list), s)
