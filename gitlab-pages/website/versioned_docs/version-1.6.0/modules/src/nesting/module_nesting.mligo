module Euro =
  struct
    type t = nat

    let add (a, b : t * t) : t = a + b

    module Coin =
      struct
        let one : t = 1n
        let two : t = 2n
      end
  end
type storage = Euro.t

let increment (s : storage) : storage =
  Euro.add (s, Euro.Coin.one)