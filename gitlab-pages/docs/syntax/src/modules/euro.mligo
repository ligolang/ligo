(* This is euro.mligo *)

module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end
type euro_balance = Euro.t

let add_tip (s : euro_balance) : euro_balance =
  Euro.add (s, Euro.one)