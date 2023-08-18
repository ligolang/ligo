type combed = [@layout comb]
  { a : int
  ; b : nat
  ; c : string
  }

let default_value = { a = 0 ; b = 1n ; c = "" }

[@entry]
let main (_, s : unit * int) : operation list * int =
  let v = default_value in
  (([] : operation list), s + v.a + v.b + String.length v.c)
