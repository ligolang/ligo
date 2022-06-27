type p = One | Two

type t = (int * p)

let s (x : t) = 
  match x with
    (n, One) -> ()
  | _        -> ()
  | (n, Two) -> ()