type combed = [@layout comb]
  { a : int
  ; b : nat
  ; c : string
  }

let default_value = { a = 0 ; b = 1n ; c = "" }

let main (_, s : unit * int) : operation list * int =
  let val = default_value in
  (([] : operation list), s + val.a + val.b + String.length val.c)
