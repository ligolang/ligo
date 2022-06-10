let main (p, s : int * int list) : operation list * int list =
  let s2 = match s with
    | x :: l -> x :: p :: x :: l
    | [] -> [p]
    in
  (([] : operation list), s2)
