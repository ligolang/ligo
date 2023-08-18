type string = int

[@entry]
let main (_, s : unit * string) : operation list * string =
  (([] : operation list), s)
