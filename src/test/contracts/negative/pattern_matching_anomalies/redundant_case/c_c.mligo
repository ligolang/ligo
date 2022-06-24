type p = Four | Five

type t = One of p | Two of nat | Three

let s (x : t) = 
  match x with
    One Five -> ()
  | One Four -> ()
  | Two c    -> ()
  | Three    -> ()
  | One Four -> ()