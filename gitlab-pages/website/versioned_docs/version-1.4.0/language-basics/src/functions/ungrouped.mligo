let k (x : int) (_ : int) = x
let k (x : int) (_y : int) = x
let closure_example (i : int) : int =
  let closure = fun (j : int) -> i + j in
  closure i