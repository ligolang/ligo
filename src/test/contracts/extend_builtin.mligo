module Tezos = struct
  let x = 42
  let f (x  : int) = x + 2
end

let y = Tezos.f Tezos.x