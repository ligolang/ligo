type t = One of int | Two of nat | Three

let s (x : t) = 
  match x with
    One a -> ()
  | _     -> ()
  | Two c -> ()
  | Three -> ()