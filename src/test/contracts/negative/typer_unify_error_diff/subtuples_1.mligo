
let main (_p, s : int * int) : operation list * int =
  let  x : int * string * (nat * tez * nat) *          tez           = 1, "a", (1n, 1tez, 1n), 1tez in
  let _y : int *          (nat * tez * int) * string * tez * address = x in
  //             ^^^^^^                ^^^    ^^^^^^         ^^^^^^^
  ([] : operation list), s
