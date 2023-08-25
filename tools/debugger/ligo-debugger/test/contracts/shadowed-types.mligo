type string = int

[@entry]
let main () (s : string) : operation list * string =
  (([] : operation list), s)
