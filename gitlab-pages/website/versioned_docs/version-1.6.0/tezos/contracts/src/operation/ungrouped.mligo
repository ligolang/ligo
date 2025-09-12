type storage = int

type parameter = Sub of int | Add of int

[@entry]
let main (p : parameter) (x : storage) : operation list * storage =
  [],
  (match p with
  | Sub i -> x - i
  | Add i -> x + i)