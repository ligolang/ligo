type p = One | Two | Three
type q = Four | Five | Six

type t = { a : int ; b : p ; c : q }

let s (x : t) = 
  match x with
    { a ; b = One;   c = Six } -> ()
  | { a ; b = Two;   c = Six } -> ()
  | { a ; b = Three; c = Six } -> ()