type r = { a : nat ; b : int ; c : string }

let r = { a = 1n ; b = 1 ; c = "Hello" }
let { a ; b ; c } = r
let { a = a1 ; b = b1 ; c = c1 } = { a = 2n ; b = 2 ; c = "World" }

type storage = nat * int * string

let main (_ : unit) (_ : storage) : operation list * storage
  = [], (a + a1, b + b1, c ^ c1)