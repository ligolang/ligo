type p = Four | Five | Six

type t = One of p | Two of nat | Three

let s (x : t) = 
  match x with
    Two _    -> ()
  | Three    -> ()
  | One Five -> ()