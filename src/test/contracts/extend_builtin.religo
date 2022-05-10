module Tezos = {
  let x = 42
  let f = (x  : int) => x + 2
}

let y = Tezos.f(Tezos.x)
