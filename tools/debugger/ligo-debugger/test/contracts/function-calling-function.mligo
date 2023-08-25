let add (a, b : int * int) : int = a + b

let complex(a, b : int * int) : int = add(a, a + b)

[@entry]
let main () (s : int) : operation list * int =
  let res = complex(s, s + 2) in
  (([] : operation list), res)
