let r = (1n, 1, "Hello")

let (a, b, c) = r

let (a1, b1, c1) = (2n, 2, "World")

type storage = nat * int * string

[@entry]
let main (_ : unit) (_ : storage) : operation list * storage =
  [], (a + a1, b + b1, c ^ c1)
