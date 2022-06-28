let rec last (xs : int list) : int = 
  match xs with
    x::[] -> x
  | _::xs -> last xs