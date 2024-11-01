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
type euro_balance = Euro.t

let increment (s : euro_balance) : euro_balance =
  Euro.add (s, Euro.Coin.one)