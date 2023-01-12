let rec foo (xs : int list) : int = 
  let rec loop (xs : int list) : int = 
    loop (foo xs :: xs)
  in
  loop xs
