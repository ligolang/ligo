module EURO = struct
  type  = nat
  let add (a, b : t * t) : t = a + b
  module CONST = struct
    let zero : t = 0n
    let one : t = 1n
  end
end

module US_DOLLAR = EURO

type storage = EURO.t

let main (action, store : unit * storage) :
  operation list * storage =
  (([] : operation list), EURO.add (store, EURO.CONST.one))
