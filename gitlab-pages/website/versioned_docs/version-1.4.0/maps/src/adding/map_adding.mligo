let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let new_map = Map.add 3 "three" my_map
let contains_3 = Map.mem 3 new_map // = true