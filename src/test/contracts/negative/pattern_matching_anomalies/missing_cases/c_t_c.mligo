type p = Four | Five | Six
type t = One of (int * p) | Two | Three

let s (x : t) = 
  match x with
    One (n, Six) -> ()
  | Two          -> ()
  | Three        -> ()