
let main (_p, s : int * int) : operation list * int =
  let x : int                = 42 in
  let y : nat * int * string = x in
  ([] : operation list), s