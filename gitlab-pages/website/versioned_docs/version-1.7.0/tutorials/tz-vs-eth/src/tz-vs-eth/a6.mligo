
type storage = int

[@entry]
let compute (func : int -> int) (s : storage) : operation list * storage =
  [], func s