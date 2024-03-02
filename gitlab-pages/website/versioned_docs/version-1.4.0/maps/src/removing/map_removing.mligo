let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let new_map = Map.remove 2 my_map
let contains_3 = Map.mem 2 new_map // = false