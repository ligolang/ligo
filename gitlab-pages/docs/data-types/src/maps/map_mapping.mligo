let my_map : (string, int) map = Map.literal [
  ("Alice", 2);
  ("Bob", 5);
  ("Charlie", 8);
]

let squared_map : (string, int) map = Map.map (fun (_k, v : string * int) : int -> v * v) my_map