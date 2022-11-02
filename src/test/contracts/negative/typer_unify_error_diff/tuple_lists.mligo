
let main (_p, s : int * int) : operation list * int =
  let x : (string * int * nat * int *       string * int) list = [ "foo" , 42  , 24n , 42 ,        "bar",  42 ] in
  let y : (tez    * int       * tez * nat * string)       list = x in
  ([] : operation list), s
