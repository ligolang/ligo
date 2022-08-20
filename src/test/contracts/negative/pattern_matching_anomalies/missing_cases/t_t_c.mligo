type p = One | Two | Three
type q = Four | Five | Six

type t = ((int * string * nat) * (p * q))

let s (x : t) = 
  match x with
    ((_, _, _), (One, Six))   -> ()
  | ((_, _, _), (Two, Six))   -> ()
  | ((_, _, _), (Three, Six)) -> ()