
let main (_p, s : int * int) : operation list * int =
  let y : {foo : int ; bar : (nat * string) ; third_field : tez} = {foo = 1 ; bar = (2n, "lol") ; third_field = 42tez } in
  let x : {foo : int ; bar : (nat * nat   )}                     = y in
  //                                ^^^^^^    ^^^^^^^^^^^^^^^^^
  ([] : operation list), s
