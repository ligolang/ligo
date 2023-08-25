type nested = [@layout comb]
  | B of int
  | A of timestamp
  | C of string

type t =
  { ch : chain_id
  ; state : nested
  ; s : string
  }

type ep =
  | Go of t
  | Nop of unit

let test_val : ep = Go ( { ch = ("NetXH12Aer3be93" : chain_id) ; state = A ("1970-01-01T00:01:40Z" : timestamp) ; s = "large" } )

[@entry]
let main () (s : int) : operation list * int =
  let v = test_val in
  let l =
    match v with
    | Go t -> String.length t.s
    | Nop -> 0n
  in
  (([] : operation list), s)
