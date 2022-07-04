type p = One | Two | Three
type q = Four | Five | Six

type t = (int * p * q)

let s (x : t) = 
  match x with
    (n, One, Six) -> ()
  | (n, Two, Six) -> ()
  | (n, Three, Six) -> ()