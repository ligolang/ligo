type p = One | Two

type t = { a : int ; b : p }

let s (x : t) = 
  match x with
    { a ; b = One } -> ()
  | _               -> ()
  | { a ; b = Two } -> ()