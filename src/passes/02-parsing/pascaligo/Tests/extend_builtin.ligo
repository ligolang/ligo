module Tezos is {
  const x = 42
  function f (const x  : int) : int is x + 2
}

const y = Tezos.f(Tezos.x)
