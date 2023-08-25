let apply (f : int -> int -> int -> int -> int) (a, b, c, d : int * int * int * int) : int =
  let applyOnce = f a in
  let applyTwice = applyOnce b in
  let applyThrice = applyTwice c in
  let result = applyThrice d in
  result

let act (a : int) (b : int) (c : int) (d : int) = a + b + c + d

[@entry]
let main () (s : int) : operation list * int =
  (([] : operation list), apply act (s, s + 2, s + 3, s + 4))
