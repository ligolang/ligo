type t = One of int | Two of nat | Three

let s (x : t) = 
  match x with
    One a -> ()
  | Two c -> ()
  | Three -> ()
  | One b -> ()