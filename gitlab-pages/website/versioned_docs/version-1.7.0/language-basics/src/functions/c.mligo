let increment (b : int) : int = (fun (a : int) -> a + 1) b
let a = increment 1 // a = 2
let incr_map (l : int list) : int list =
  List.map (fun (i : int) -> i + 1) l