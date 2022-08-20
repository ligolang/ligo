type p = One | Two

type t = { a : int ; b : p }

let s (x : t) = 
  match x with
    _               -> ()
  | { a ; b = One } -> ()
  | { a ; b = Two } -> ()