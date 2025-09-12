module EURO =
  struct
    type t = nat
    let add (a , b : t * t) : t = a + b
    let zero : t = 0n
    let one : t = 1n
  end
type storage = EURO.t

[@entry]
let main (_action : unit) (store : storage) : operation list * storage =
  [], EURO.add (store, EURO.one)
module US_DOLLAR = EURO