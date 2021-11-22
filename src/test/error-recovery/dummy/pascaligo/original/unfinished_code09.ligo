module EURO is {
  type t is nat
  function add (const a : t; const b : t) : t is a + b
  module CONST is begin
    const zero : t = 0n
    const one : t = 1n
  end
}

module US_DOLLAR is EURO

type storage is EURO.t


function main (const action : unit; const store : storage) : (list (operation)) * storage is
  ((nil : list (operation)), EURO.add(store, EURO.CONST.one))
