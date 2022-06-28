type t = One of int | Two of nat | Three

let s (x : t) = 
  match x with
    One a -> ()
  | One b -> ()
  | Two c -> ()
  | Three -> ()