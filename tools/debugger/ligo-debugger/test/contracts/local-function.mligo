[@entry]
let main () (s : int) : operation list * int =
  // TODO: make this function curried when location meta for `x + y` is fixed.
  let f (x, y : int * int) : int = x + y in
  let a = 42 in
  let b = s + 2 in
  (([] : operation list), f (a, b))
