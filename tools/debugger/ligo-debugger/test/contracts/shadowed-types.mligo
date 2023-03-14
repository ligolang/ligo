type string = int

let main (_, s : unit * string) : operation list * string =
  (([] : operation list), s)
