type test =
  | Alt1 of int -> string
  | Alt2
  | Alt3 of {field1: int; field2: int * int -> (Join of int | Empty)}

let main (p, _ : unit * unit) : operation list * unit =
  ([] : operation list), p
