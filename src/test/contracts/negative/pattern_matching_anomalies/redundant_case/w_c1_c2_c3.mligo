type t = One of int | Two of nat | Three

let s (x : t) = 
  match x with
    _     -> ()
  | One a -> ()
  | Two c -> ()
  | Three -> ()