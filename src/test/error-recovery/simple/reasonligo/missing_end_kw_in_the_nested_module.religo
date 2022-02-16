module EURO = {
  type t = nat
  let add = (a, b : (t, t)) : t => a + b
  module CONST = {
    let zero : t = 0n
    let one : t = 1n
}

module US_DOLLAR = EURO

type storage = EURO.t

let main = (action, store : (unit, storage)) :
  (list (operation), storage) =>
  (([] : list(operation)), EURO.add (store, EURO.CONST.one))
