type p = One | Two | Three | Four
type r = { d : { e : string ; f : int } ; i : { g : p ; h : nat } }

type t = { a : int ; b : r ; c : nat }

let s (x : t) = 
  match x with
    { a ; b = { d = { e ; f } ; i = { g = Two ; h } } ; c }   -> ()
  | { a ; c ; b = { d = { e ; f } ; i = { g = Three ; h } } } -> ()