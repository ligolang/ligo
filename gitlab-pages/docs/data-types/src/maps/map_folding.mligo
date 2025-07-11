let my_map : (string, nat) map = Map.literal [
  ("Alice", 1n);
  ("Bob", 4n);
  ("Charlie", 5n);
]

let fold_function = fun (acc, element : nat * (string * nat)) ->
  let _key, value = element in
  acc + value

let map_sum = Map.fold fold_function my_map 0 (* 10 *)