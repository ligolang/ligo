type p = Four | Five | Six
type t = One of { a : int ; b : p } | Two | Three

let s (x : t) = 
  match x with
    One { a ; b = Six } -> ()
  | Two                 -> ()
  | Three               -> ()