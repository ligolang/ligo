let weird_length (v : int list) : int =
  match v with
  | [] -> -1
  | [ a; b ; c] -> -2
  | x -> int (List.length x)